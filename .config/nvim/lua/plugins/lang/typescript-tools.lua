return {
	{
		'pmizio/typescript-tools.nvim',
		-- enabled = false, -- just not ready for prime time yet
		ft = { 'typescriptreact', 'typescript', 'javascript', 'javascriptreact' },
		dependencies = { 'nvim-lua/plenary.nvim', 'neovim/nvim-lspconfig' },
		opts = function()
			return {
				handlers = {
					['textDocument/publishDiagnostics'] = require('typescript-tools.api').filter_diagnostics {
						-- Ignore 'This may be converted to an async function' diagnostics.
						80006,
						-- unused
						6196,
					},
				},
				settings = {
					tsserver_plugins = U.project('~/work/deals-light-ui', {
						'@styled/typescript-styled-plugin',
					}),
					tsserver_file_preferences = {
						includeInlayVariableTypeHints = true,
						includeInlayFunctionLikeReturnTypeHints = true,
					},
				},
				on_attach = function(_, buffer)
					local augroup = U.augroups.lsp_formatting
					vim.api.nvim_clear_autocmds { group = augroup, buffer = buffer }
					vim.api.nvim_create_autocmd('BufWritePre', {
						group = augroup,
						buffer = buffer,
						callback = function()
							-- local ts = require('typescript').actions
							-- ts.removeUnused { sync = true }
							-- ts.addMissingImports({ sync = true })
							-- ts.organizeImports { sync = true }
							vim.cmd [[Format]]
						end,
					})
					-- map <leader>ll yiwoconsole.log('\n<C-r>0:', <C-r>0);<C-[>k
					-- map <leader>ld :%s/.*console.log.*\n//g<CR>
					U.keys(buffer, {
						{
							'g',
							function() require('codethread.find_node_module').find_node_module() end,
							'Find Modules',
						},
						{
							'cc',
							Cmd '%g/console/norm dd',
							'Clear logs',
						},
						{
							's',
							function() require('swap-ternary').swap() end,
							'Swap ternary',
						},
					})
				end,
			}
		end,
	},
}
