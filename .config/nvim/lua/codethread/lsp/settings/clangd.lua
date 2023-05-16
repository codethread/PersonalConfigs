return {
	filetypes = { 'c', 'cpp', 'objc', 'objcpp', 'cuda' },
	on_attach = function(client, bufnr)
		require('codethread.lsp.settings.shared').on_attach(client, bufnr)
	end,
}
