require('neodev').setup {}

return {
	settings = {
		Lua = {
			completion = {
				callSnippet = 'Replace',
			},
		},
	},
	on_attach = function(client, bufnr) require('codethread.lsp.settings.shared').on_attach(client, bufnr) end,
}
