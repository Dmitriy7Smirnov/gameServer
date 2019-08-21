{application, server, [
    {description,"Description"},
    {vsn,"1.0"},
    {modules,[server_sup,server_app,server,fighter_fun]},
    {registered,[]},
    {applications,[kernel,stdlib, client]},
	{mod,{server_app,[]}} 
]}.
