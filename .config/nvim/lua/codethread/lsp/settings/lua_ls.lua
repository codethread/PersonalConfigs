-- require('neodev').setup {
-- 	override = function(root_dir, library)
-- 		P { root_dir = root_dir, library = library }

-- 		-- if require('neodev.util').has_file(root_dir, '.dottyignore') then
-- 		print 'in dots'
-- 		library.enabled = true
-- 		library.plugins = true
-- 		-- end
-- 	end,
-- }

return {
	-- settings = {
	-- 	Lua = {
	-- 		completion = {
	-- 			callSnippet = 'Replace',
	-- 		},
	-- 	},
	-- },
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
			},
			-- Do not send telemetry data containing a randomized but unique identifier
			telemetry = {
				enable = false,
			},
		},
	},
	on_attach = function(client, bufnr)
		require('codethread.lsp.settings.shared').on_attach(client, bufnr)
	end,
}
