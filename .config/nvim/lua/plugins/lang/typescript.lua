return {
	-- U.tools_null { 'prettierd' },
	{
		'neovim/nvim-lspconfig',
		-- enabled = false, -- deprecate in favour of typescript-tools
		dependencies = {
			'jose-elias-alvarez/typescript.nvim',
			'marilari88/twoslash-queries.nvim',
		},
		opts = function()
			local nvim_lsp = require 'lspconfig'
			local root = nvim_lsp.util.root_pattern

			---@type PluginLspOpts
			return {
				servers = {
					tailwindcss = {
						root_dir = root 'tailwind.config.js',
						settings = {
							-- TODO: turn this off based on project, probably needs to look for the tailwind config
							tailwindCSS = {
								experimental = {
									classRegex = {
										{
											'tv\\((([^()]*|\\([^()]*\\))*)\\)',
											'["\'`]([^"\'`]*).*?["\'`]',
										},
									},
								},
							},
						},
					},
					eslint = {
						settings = {
							run = 'onSave',
						},
					},
					denols = {
						root_dir = root('deno.json', 'deno.jsonc'),
					},
					ts_ls = {
						root_dir = root 'package.json',
						single_file_support = false, -- XXX: copied from deno, not sure if i want this or not
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
					denols = function(_, opts) end,
					ts_ls = function(_, opts)
						U.lsp_attach('ts_ls', function(client, buffer)
							require('twoslash-queries').attach(client, buffer)

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
								{
									'a',
									function()
										require('typescript').actions.addMissingImports {
											sync = true,
										}
									end,
									'add missing imports',
								},
								{
									'o',
									function() require('typescript').actions.organizeImports { sync = true } end,
									'organise imports',
								},
								{
									',',
									function() require('typescript').actions.removeUnused { sync = true } end,
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
			}
		end,
	},
	{},
}
