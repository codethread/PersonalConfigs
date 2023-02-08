require('typescript').setup {
	disable_commands = false, -- prevent the plugin from creating Vim commands
	debug = false, -- enable debug logging for commands
	go_to_source_definition = {
		fallback = true, -- fall back to standard LSP definition on failure
	},
	server = { -- pass options to lspconfig's setup method
		init_options = { -- FIX: either ignored or not working, gets passed in, so not sure, should try other options
			plugins = {},
			preferences = {
				quotePreference = 'single',
			},
		},
		on_attach = function(client, bufnr)
			require('codethread.lsp.settings.shared').on_attach(client, bufnr)
		end,
	},
	-- root_dir = require('lspconfig.util').root_pattern 'yarn.lock',
}
