return {
	{
		'pmizio/typescript-tools.nvim',
		ft = { 'typescriptreact', 'typescript', 'javascript', 'javascriptreact' },
		dependencies = { 'nvim-lua/plenary.nvim', 'neovim/nvim-lspconfig' },
		opts = {
			settings = {
				tsserver_plugins = U.project('~/work/deals-light-ui', {
					'@styled/typescript-styled-plugin',
				}),
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
						'cc',
						Cmd '%g/console/norm dd',
						'Clear logs',
					},
				})
			end,
		},
	},
}
