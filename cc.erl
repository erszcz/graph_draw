-module(cc).

-export([cc_components/1]).

-import(digraph, [add_vertex/3,
		  del_edge/2,
		  edge/2,
		  in_neighbours/2,
		  out_edges/2,
		  out_neighbours/2,
		  vertex/2,
		  vertices/1]).

-import(lists, [map/2,
		foldl/3,
		foreach/2]).


%% from Thomas Arts. Thanks!!

% For multiple use of the algorithm, one should be
%     able to reset all the vertices to the same colour
%     with timestamp zero.
%
reset_colour(Graph,Col)
  -> map(fun(V) -> add_vertex(Graph,V,Col) end, vertices(Graph)).

% Graph is coloured black with timestamp in every vertex
% All vertices on a component are marked by a component number
% (instead of a colour)
% 
cc_components(Graph)
  -> reset_colour(Graph,{0,white}),
     foldl(fun(V,T) -> colouring(Graph,V,T) end,1,vertices(Graph)),
     components(Graph,0).

% The colouring of a graph is done as follows:
%
%  find a white vertex in the graph
%  recursively : colour it gray with timestamp
%                increase time
%                find a white neighbour of this vertex
%  until no white neighbours anymore
%
colouring(Graph,Vertex,Time)
  -> case vertex(Graph,Vertex) of
          {_,{_,white}}
            -> add_vertex(Graph,Vertex,{Time,gray}),
               NewTime = colour_neighbours(Graph,Vertex,Time+1),
               add_vertex(Graph,Vertex,{NewTime,black}),
               NewTime+1;
          _ -> Time
     end.

colour_neighbours(Graph,Vertex,Time)
  -> NVs = out_neighbours(Graph,Vertex),
     foldl(fun(V,T) -> colouring(Graph,V,T) end,Time,NVs).

%
% After that the graph is completely coloured black with
%       timestamp to every vertexe, the components are found
%       by starting at maximal timestamped vertex and doing a 
%       breathfirst search in the reversed direction of the arrows.
%
components(Graph,N)
  -> case pick_maximum(Graph,vertices(Graph),0,none) of
          none
            -> [];
          V -> Comp = reverse_connected(Graph,[],V,N),
               unconnect(Graph,V,N),          % remove edges here
               [Comp|components(Graph,N+1)]
     end.

pick_maximum(Graph,[],MaxSoFar,MaxV)
  -> MaxV;
pick_maximum(Graph,[V|Vs],MaxSoFar,MaxV)
  -> case vertex(Graph,V) of
          {_,{Time,black}}
            -> case Time > MaxSoFar of
                    true
                      -> pick_maximum(Graph,Vs,Time,V);
                    false
                      -> pick_maximum(Graph,Vs,MaxSoFar,MaxV)
               end;
          _ -> pick_maximum(Graph,Vs,MaxSoFar,MaxV)
     end.


% reverse connected marks recursively all vertices
%         that are reversed connected to the vertex
%         with the given component number
%
reverse_connected(Graph,Comp,Vertex,N)
  -> case vertex(Graph,Vertex) of
          {_,{_,black}}
            -> add_vertex(Graph,Vertex,{comp,N}),
               foldl(fun(V,C)
                         -> reverse_connected(Graph,C,V,N)
                     end,[Vertex|Comp],in_neighbours(Graph,Vertex));
          _ -> Comp
     end.

% After a component has been marked, every edge to a
%       vertex not in the component may be removed
%
unconnect(Graph,Vertex,N)
  -> add_vertex(Graph,Vertex,{cc,N}),
     foreach(fun(E)
                 -> {_,_,V2,Data} = edge(Graph,E),
                    case vertex(Graph,V2) of
                         {_,{comp,N}}
                           -> unconnect(Graph,V2,N);
                         {_,{cc,N}}
                           -> void;
                         _ -> del_edge(Graph,E),
                              void
                    end
              end,out_edges(Graph,Vertex)).



