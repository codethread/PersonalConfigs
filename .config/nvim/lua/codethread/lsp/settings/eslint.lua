return {
	settings = {
		run = 'onSave',
	},

	on_attach = function(client) client.server_capabilities.document_formatting = false end,
	root_dir = require('lspconfig.util').root_pattern 'yarn.lock',
}
