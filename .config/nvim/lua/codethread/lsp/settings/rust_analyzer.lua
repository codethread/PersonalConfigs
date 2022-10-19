return {
	settings = {
		['rust-analyzer'] = {
			checkOnSave = {
				allFeatures = true,
				command = 'clippy',
			},
		},
	},

	on_attach = function(client, bufnr) require('codethread.lsp.settings.shared').on_attach(client, bufnr) end,
}
