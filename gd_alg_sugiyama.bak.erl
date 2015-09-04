-module(gd_alg_sugiyama).
-author('hans@erix.ericsson.se').

-export([design/3]).

-import(lists, [map/2, foreach/2, max/1, min/1,	foldl/3, foldr/3]).

-import(ordsets, [from_list/1, is_element/2, add_element/2, del_element/2,
		  subtract/2, union/2, intersection/2]).

-import(gd_lib, [zip/2, unzip/1, addD/2]).


-include("gd.hrl").
-include("gd_algs.hrl").

%%% A classical graph placement algorithm.


design(Roots, Graph, Opt) ->
    NumLevels = assign_levels(Roots, Graph, Opt),
    insert_extra_nodes(Graph),
    Levels = initialize_bcs(Graph),
    Nchanges = first_bc_passes(Graph, Levels),
    more_bc_passes(Graph, Levels, Nchanges),
    %% temporary position-in-level assignment:
    BCs = map(
	    fun(V) ->
		    {_,D} = digraph:vertex(Graph, V),
		    {B1,B2} = D#vertex.bc,
		    (B1+B2) / 2
	    end, digraph:vertices(Graph)),
    Min = min(BCs),
    Max = max(BCs),
    io:format('Min= ~w, Max= ~w\n', [Min,Max]),
    F = if
	    Opt#gd_options.horizontal==true -> 
		fun({B1,B2}) -> M = (B1+B2) / 2,
				(M-Min) * 200 / (Max-Min)
		end;

	   true ->
		fun({B1,B2}) -> M = (B1+B2) / 2,
				(M-Min) * 600 / (Max-Min)
		end
	end,

    foreach(
      fun(V) ->
	      {_,D} = digraph:vertex(Graph, V),
	      Pos = F(D#vertex.bc),
	      digraph:add_vertex(Graph, V, D#vertex{level_pos=Pos})
      end, digraph:vertices(Graph)).



%%%----------------
assign_levels(Roots, Graph, Opt) -> 
    assign_levels(from_list(Roots), Graph, Opt, 0).


assign_levels([], _, _, N) ->
    N;

assign_levels(Roots, Graph, Opt, N) when Opt#gd_options.directed==true ->
    ThisLevel = foldl(
		  fun(V,Lev) ->
			  INs = from_list(digraph:in_neighbours(Graph,V)),
			  case intersection(INs,Lev) of
			      [] -> Lev;
			      _ -> del_element(V,Lev)
			  end
		  end, Roots, Roots),
%%    io:format('Level ~w: Roots=~p ThisLevel=~p\n',[N,Roots,ThisLevel]),
    foreach(fun(V) ->
		    {_,D} = digraph:vertex(Graph,V),
		    digraph:add_vertex(Graph,V,D#vertex{level=N})
	    end, ThisLevel),
    Children = foldl(
		 fun(V,Acc) ->
			 ONs = from_list(digraph:out_neighbours(Graph,V)),
			 union(ONs,Acc)
		 end, [], ThisLevel),
    assign_levels(union(Children, subtract(Roots,ThisLevel)),
		  Graph, Opt, N+1).


%%%----------------
insert_extra_nodes(Graph) ->
    foldl(
      fun(E,N) ->
	      {_,F,T,D} = digraph:edge(Graph,E),
	      {_,Df} = digraph:vertex(Graph,F),
	      {_,Dt} = digraph:vertex(Graph,T),
	      case (Dt#vertex.level-Df#vertex.level) of
		  1 -> 
		      digraph:add_edge(Graph,E,F,T,D#edge{type=placed}),
		      N;
		  I when integer(I), I>1 ->	% insert extra node(s)
		      break_edge(Df#vertex.level, Dt#vertex.level,
				 F, T, Graph, N, last),
		      digraph:del_edge(Graph,E),
		      N + I-1;

		  -1 ->				% backward edge, reverse it
		      digraph:del_edge(Graph,E),
		      digraph:add_edge(Graph,{T,F},T,F,
				       D#edge{arrow_place=first,
					      type=placed}),
		      N;
		  I when integer(I), I< -1 ->	% long backward edge
		      break_edge(Dt#vertex.level, Df#vertex.level,
				 T, F, Graph, N, first),
		      digraph:del_edge(Graph,E),
		      N + I-1
	      end
      end, 0, digraph:edges(Graph)).
    

break_edge(LevelF,LevelT, F,T, Graph, N, Apos) ->
    NewNames =
	  map(fun(Lev) ->
		      New = ?placeholder(N+Lev-LevelF-1),
		      digraph:add_vertex(Graph,New,#vertex{level=Lev}),
		      New
	      end, lists:seq(LevelF+1,LevelT-1)),
    LastName =
	foldl(
	  fun(Name,PrevName) ->
		  digraph:add_edge(Graph, {PrevName,Name}, PrevName,Name,
				   #edge{arrow_place=Apos,
					 type=placed}),
		  Name
	  end, F, NewNames),
    digraph:add_edge(Graph, {LastName,T}, LastName,T, #edge{arrow_place=Apos,
							    type=placed}).


%%%----------------------------------------------------------------
initialize_bcs(Graph) ->
    get_neighbours(sort_by_level(Graph), Graph).


%%%----
sort_by_level(Graph) ->
    Nodes = 
	lists:keysort(1, map(fun(V) ->
				     {_,D} = digraph:vertex(Graph,V),
				     {D#vertex.level, V}
			     end, digraph:vertices(Graph))),
    Levels = collect_levels(Nodes, [], none),
    foreach(
      fun(Ns) ->
	      foldl(
		fun(V,N) -> 
			{_,D} = digraph:vertex(Graph,V),
			digraph:add_vertex(Graph,V,D#vertex{bc={N,N}}),
			N+1
		end, 1, Ns)
      end, Levels),
    Levels.
    
		   
collect_levels([{L,V}|Ns], Acc, L) ->
    collect_levels(Ns, [V|Acc], L);

collect_levels([{L,V}|Ns], Acc, Lold) when integer(Lold) ->
    [Acc | collect_levels(Ns,[V],L)];

collect_levels([{L,V}|Ns], Acc, none) ->
    collect_levels(Ns, [V], L);

collect_levels([], Acc, _) ->    
    [Acc].

%%%----
get_neighbours(Ls, Graph) ->
    map(fun(Ns) ->
		map(fun(V) -> {V, 
			       digraph:out_neighbours(Graph,V),
			       digraph:in_neighbours(Graph,V)}
		    end, Ns)
	end, Ls).

%%%----------------------------------------------------------------
first_bc_passes(Graph, Levels) ->
%%    io:format('Levels,RevLevels = ~p\n', [Levels]),
    foreach(
      fun(Ns) ->
	      foreach(
		fun({V,INs,OUTs}) -> 
			{_,D} = digraph:vertex(Graph,V),
			{B1,B2} = D#vertex.bc,
			P = mean_bcs(INs, B1, Graph),
			digraph:add_vertex(Graph,V,D#vertex{bc={P,B2}})
		end, Ns)
      end, Levels),
%%    io:format('----------------\n', []),
    foreach(
      fun(Ns) ->
	      foreach(
		fun({V,INs,OUTs}) -> 
			{_,D} = digraph:vertex(Graph,V),
			{B1,B2} = D#vertex.bc,
			P = mean_bcs(OUTs, B2, Graph),
%%			io:format('~w ~w ~w\n', [V,B1,B2]),
			digraph:add_vertex(Graph,V,D#vertex{bc={B1,P}})
		end, Ns)
      end, Levels),
    Levels.


mean_bcs([], Default, _) ->
    Default;
mean_bcs(L, _, Graph) ->
    Sum2 =
	foldl(fun(V,Acc) ->
		      {_,D} = digraph:vertex(Graph,V),
		      {B1,B2} = D#vertex.bc,
		      B1+B2+Acc
	      end, 0, L),
    Sum2 / (2*length(L)).
    
		  
more_bc_passes(Graph, Levels, Nchanges) ->
    foreach(fun(_) ->
		    first_bc_passes(Graph, Levels)
	    end, lists:seq(1,10)).
