return {

	on_attach = function(client, bufnr)
		-- use prettier for formatting
		client.resolved_capabilities.document_formatting = false

		require("codethread.lsp.settings.shared").lsp_highlight_document(client)
		require("codethread.lsp.settings.shared").lsp_keymaps(bufnr)
	end,
}
