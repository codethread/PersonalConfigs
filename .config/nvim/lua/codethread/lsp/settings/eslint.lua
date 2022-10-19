return {
	settings = {
		run = 'onSave',
	},

	on_attach = function(client) client.server_capabilities.document_formatting = false end,
}
