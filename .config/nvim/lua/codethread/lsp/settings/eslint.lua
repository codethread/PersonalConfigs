return {
	settings = {
		run = "onSave",
	},

	on_attach = function(client)
		client.resolved_capabilities.document_formatting = false
	end,
}
