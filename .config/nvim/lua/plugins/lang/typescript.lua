return {
	-- U.tools_null { 'prettierd' },
	{
		'neovim/nvim-lspconfig',
		-- enabled = false, -- deprecate in favour of typescript-tools
		dependencies = {
			'jose-elias-alvarez/typescript.nvim',
			'marilari88/twoslash-queries.nvim',
		},
		opts = {
			-- make sure mason installs the server
			servers = {
				---@type lspconfig.options.tsserver
				tsserver = {
					settings = {
						typescript = {
							format = {
								indentSize = vim.o.shiftwidth,
								convertTabsToSpaces = vim.o.expandtab,
								tabSize = vim.o.tabstop,
							},
						},
						javascript = {
							format = {
								indentSize = vim.o.shiftwidth,
								convertTabsToSpaces = vim.o.expandtab,
								tabSize = vim.o.tabstop,
							},
						},
						completions = {
							completeFunctionCalls = true,
						},
					},
				},
			},
			setup = {
				tsserver = function(_, opts)
					U.lsp_attach('tsserver', function(client, buffer)
						require('twoslash-queries').attach(client, buffer)

						U.keys(buffer, {
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
							{
								'a',
								function()
									require('typescript').actions.addMissingImports { sync = true }
								end,
								'add missing imports',
							},
							{
								'o',
								function()
									require('typescript').actions.organizeImports { sync = true }
								end,
								'organise imports',
							},
							{
								',',
								function()
									require('typescript').actions.removeUnused { sync = true }
								end,
								'Remove unused',
							},
						})
						local augroup = U.augroups.lsp_formatting
						vim.api.nvim_clear_autocmds { group = augroup, buffer = buffer }
						vim.api.nvim_create_autocmd('BufWritePre', {
							group = augroup,
							buffer = buffer,
							callback = function() vim.cmd [[Format]] end,
						})
						-- map <leader>ll yiwoconsole.log('\n<C-r>0:', <C-r>0);<C-[>k
						-- map <leader>ld :%s/.*console.log.*\n//g<CR>
					end)
					require('typescript').setup { server = opts }
					return true
				end,
			},
		},
	},
	{},
}
