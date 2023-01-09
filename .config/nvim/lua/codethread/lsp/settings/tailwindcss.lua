return {
	settings = {},

	on_attach = function(client, bufnr)
		require('codethread.lsp.settings.shared').on_attach(client, bufnr)
	end,
	root_dir = require('lspconfig.util').root_pattern('tailwind.config.js', 'tailwind.config.ts'),
}
