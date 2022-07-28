return {
	settings = {
		['rust-analyzer'] = {
			checkOnSave = {
				allFeatures = true,
				command = 'clippy',
			},
		},
	},

	on_attach = function(client, bufnr)
		-- turn off formatting as we'll use rustfmt
		client.resolved_capabilities.document_formatting = false

		require('codethread.lsp.settings.shared').lsp_highlight_document(client)
		require('codethread.lsp.settings.shared').lsp_keymaps(bufnr)

		require('nvim-navic').attach(client, bufnr)
	end,
}
