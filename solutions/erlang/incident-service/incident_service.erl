-module(incident_service).
-include("../lib/yaws/include/yaws_api.hrl").
-export([out/1]).


out(Arg) ->
    Uri = yaws_api:request_url(Arg),
    Method = method(Arg),
    Path = string:tokens(Uri#url.path, "/"),

    io:format("~p:~p ~p Request ~n", [?MODULE, ?LINE, Method]),
    handle(Method, Path, Arg).

method(Arg) ->
  Rec = Arg#arg.req,
  Rec#http_request.method.

handle('GET', _, _Arg) ->
    io:format("~n ~p:~p GET Request ~n", [?MODULE, ?LINE]),
    io:format("~n ~p:~p GET Request Response ~n", [?MODULE, ?LINE]),
    {html};

handle(Method, Path, _) ->
    [{error, "Unknown method " ++ Method ++ " (" ++ Path ++ ")"},
     {status, 405},
     {header, "Allow: GET, HEAD, POST, PUT, DELETE"}
     ].


% /actuator/health

