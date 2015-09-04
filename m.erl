-module(m).
-author('hans@erix.ericsson.se').
-include_lib("kernel/include/file.hrl").
-export([l/0]).

l() -> lists:foreach(
	 fun(Module) ->
		 load_if_new(Module)
	 end, loaded_modules()),
       io:nl().
    
load_if_new(Module) ->
    File = object_file(Module),
    case filename:absname(filename:basename(File)) of
	File ->
	    Tf = file_time(File),
	    Tm = loaded_time(Module),
	    if
		Tm == Tf ->
		    io:format('(~w) ', [Module]),
		    ok;
		true ->
%%		    io:format('Loading ~w   ~w  ~w ~s..', [Module,Tm,Tf,File]),
		    io:format('~w ', [Module]),
		    c:l(Module)
	    end;
	_ ->
%%	    io:format('Skipping ~s\n', [File]),
	    ok
    end.



file_time(File) ->
    if atom(File) ->
	    unknown;
       true ->
	    {ok,R} = file:read_file_info(File),
	    {{Year,Month,Date},{Hr,Min,Sec}} = R#file_info.atime,
	    {Year,Month,Date,Hr,Min}
    end.
    

loaded_time(Module) ->
    case lists:keysearch(time,1,Module:module_info(compile)) of
	{value, {time, {Year,Month,Date,Hr,Min,Sec}}} ->
	    {Year,Month,Date,Hr,Min};
	_ ->
	    unknown
    end.
	    

loaded_modules() -> lists:map(fun({Module,_}) ->
				      Module
			      end, code:all_loaded()).


object_file(Module) ->
    {value, {_,File}} =
	lists:keysearch(Module,1,code:all_loaded()),
    File.
