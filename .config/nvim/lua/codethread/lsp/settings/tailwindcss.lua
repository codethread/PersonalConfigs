return {
	settings = {},

	on_attach = function(client, bufnr)
		-- turn off formatting as we'll use prettier
		-- client.resolved_capabilities.document_formatting = false

		require('codethread.lsp.settings.shared').lsp_highlight_document(client)
		require('codethread.lsp.settings.shared').lsp_keymaps(bufnr)
	end,
}
