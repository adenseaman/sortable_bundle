{application, distmap,
	[{vsn, "0.1.0"},
	{modules, [calculator, client_coordinator, server_coordinator, server_supervisor, utils, distmap]},
	{registered, [distmap]},
	{mod, {distmap, []}}
]}.