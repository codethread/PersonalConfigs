return {
	settings = {
		json = {
			schemas = require('schemastore').json.schemas(),
			validate = { enable = true },
		},
	},

	on_attach = function(client)
		client.resolved_capabilities.document_formatting = false
	end,
}
