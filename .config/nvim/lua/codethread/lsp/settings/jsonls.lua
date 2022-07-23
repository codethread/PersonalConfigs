return {
	settings = {
		json = {
			schemas = require("schemastore").json.schemas(),
		},
	},

	on_attach = function(client)
		client.resolved_capabilities.document_formatting = false
	end,
}
