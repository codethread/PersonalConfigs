return {

	on_attach = function(client, bufnr)
		require('codethread.lsp.settings.shared').lsp_highlight_document(client)
		require('codethread.lsp.settings.shared').lsp_keymaps(bufnr)
	end,
}
