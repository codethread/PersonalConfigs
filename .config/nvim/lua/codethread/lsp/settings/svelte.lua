return {
	settings = {},

	on_attach = function(client, bufnr)
		require('codethread.lsp.settings.shared').on_attach(client, bufnr)
	end,
}
