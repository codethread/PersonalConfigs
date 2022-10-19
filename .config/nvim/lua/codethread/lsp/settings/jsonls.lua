return {
	settings = {
		json = {
			schemas = require('schemastore').json.schemas(),
			validate = { enable = true },
		},
	},

	on_attach = function(client, bufnr) require('codethread.lsp.settings.shared').on_attach(client, bufnr) end,
}
