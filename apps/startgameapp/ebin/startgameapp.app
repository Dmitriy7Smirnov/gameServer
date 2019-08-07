{application, startgameapp, [
    {description,"Description"},
    {vsn,"1.0"},
    {modules,[startgamesup,startgameapp,myapp_sup,myapp_app]},
    {registered,[]},
    {applications,[kernel,stdlib,serverapp,clientapp]},
	{mod,{startgameapp,[]}} 
]}.
