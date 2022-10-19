-- local List = require 'pl.List'
return {
	settings = {
		Lua = {
			runtime = {
				-- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
				version = 'LuaJIT',
			},
			diagnostics = {
				-- Get the language server to recognize the `vim` global
				globals = { 'vim' },
			},
			workspace = {
				-- Make the server aware of Neovim runtime files
				library = vim.api.nvim_get_runtime_file('', true),
				-- library = List(vim.api.nvim_get_runtime_file('', true)):extend {
				-- 	-- TODO: get this info in smarter way
				-- 	vim.fn.stdpath 'cache' .. '/packer_hererocks/2.1.0-beta3/share/lua/5.1',
				-- },
			},
			-- Do not send telemetry data containing a randomized but unique identifier
			telemetry = {
				enable = false,
			},
		},
	},
	on_attach = function(client, bufnr) require('codethread.lsp.settings.shared').on_attach(client, bufnr) end,
}
