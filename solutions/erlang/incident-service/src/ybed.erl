-module(ybed).
-export([start/0]).
-export([run/0]).

start() ->
    {ok, spawn(?MODULE, run, [])}.

run() ->
    GconfList = [{ebin_dir, ["/home/ezivkoc/repo-ziver/workshop-hackathon/solutions/erlang/lib/yaws/ebin"]}],
    Docroot = "/home/ezivkoc/repo-ziver/workshop-hackathon/solutions/erlang/lib/yaws/test",
    SconfList = [{port, 8080},
                 {listen, {0,0,0,0}},
                 {docroot, Docroot},
                 {appmods, [{"/", incident_service}]}
                ],
    {ok, SCList, GC, ChildSpecs} =
        yaws_api:embedded_start_conf(Docroot, SconfList, GconfList),
    [supervisor:start_child(ybed_sup, Ch) || Ch <- ChildSpecs],
    yaws_api:setconf(GC, SCList),
    {ok, self()}.