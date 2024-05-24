vim.opt.completeopt = { 'menu', 'menuone', 'noselect' }

return {
	-- snippets
	{
		'L3MON4D3/LuaSnip',
		version = 'v2.*',
		dependencies = 'rafamadriz/friendly-snippets',
		build = 'make install_jsregexp',
		config = function()
			local ls = require 'luasnip'
			ls.setup {
				history = true,
				delete_check_events = 'TextChanged',
			}
			require('luasnip.loaders.from_vscode').lazy_load {
				paths = {
					'~/.config/nvim/snippets_vscode',
					'~/.local/share/nvim/lazy/friendly-snippets',
				},
			}

			vim.keymap.set({ 'i', 's' }, '<Tab>', function() ls.expand() end, { silent = true })
			vim.keymap.set({ 'i', 's' }, '<C-n>', function() ls.jump(1) end, { silent = true })
			vim.keymap.set({ 'i', 's' }, '<C-p>', function()
				if ls.jumpable(-1) then ls.jump(-1) end
			end, { silent = true })

			-- vim.keymap.set({ 'i', 's' }, '<C-E>', function()
			-- 	if ls.choice_active() then ls.change_choice(1) end
			-- end, { silent = true })
		end,
	},

	-- cmp and friends
	{
		'onsails/lspkind.nvim',
		lazy = true,
		config = function()
			local lspkind = require 'lspkind'
			lspkind.init {
				symbol_map = {
					Copilot = ' ',
					Text = '󰉿 ',
					Method = '󰆧 ',
					Function = '󰊕 ',
					Constructor = ' ',
					Field = '󰜢 ',
					Variable = '󰀫 ',
					Class = '󰠱 ',
					Interface = ' ',
					Module = ' ',
					Property = '󰜢 ',
					Unit = '󰑭 ',
					Value = '󰎠 ',
					Enum = ' ',
					Keyword = '󰌋 ',
					Snippet = ' ',
					Color = '󰏘 ',
					File = '󰈙 ',
					Reference = '󰈇 ',
					Folder = '󰉋 ',
					EnumMember = ' ',
					Constant = '󰏿 ',
					Struct = '󰙅 ',
					Event = ' ',
					Operator = '󰆕 ',
					TypeParameter = ' ',
				},
			}
		end,
	},
	{
		'hrsh7th/nvim-cmp',
		version = false, -- last release is way too old
		event = 'InsertEnter',
		dependencies = {
			'hrsh7th/cmp-nvim-lsp',
			'hrsh7th/cmp-cmdline',
			'hrsh7th/cmp-buffer',
			'hrsh7th/cmp-path',
			'hrsh7th/cmp-nvim-lsp-signature-help',
			{
				'saadparwaiz1/cmp_luasnip',
				dependencies = 'L3MON4D3/LuaSnip',
			},
			'zbirenbaum/copilot-cmp',
		},
		config = function()
			local cmp = require 'cmp'
			cmp.setup {
				snippet = {
					expand = function(args) require('luasnip').lsp_expand(args.body) end,
				},
				mapping = {
					['<C-k>'] = cmp.mapping.select_prev_item(),
					['<C-j>'] = cmp.mapping.select_next_item(),
					['<C-l>'] = cmp.mapping.confirm { select = true }, -- select grabs first item if none were selected
					['<Esc>'] = cmp.mapping.abort(),
					['<C-c>'] = cmp.mapping.abort(),
					['<C-u>'] = cmp.mapping.scroll_docs(-4),
					['<C-d>'] = cmp.mapping.scroll_docs(4),
				},
				completion = {
					keyword_length = 2,
					-- keyword_length = 1000,
				},
				sources = cmp.config.sources {
					{ name = 'nvim_lsp' },
					{ name = 'copilot' },
				},
				formatting = {
					fields = { 'kind', 'abbr', 'menu' },
					format = function(entry, vim_item)
						local kind = require('lspkind').cmp_format {
							mode = 'symbol_text',
							maxwidth = 50,
						}(entry, vim_item)
						local strings = vim.split(kind.kind, '%s', { trimempty = true })
						kind.kind = ' ' .. (strings[1] or '') .. ' '
						local m = strings[2]
						if m == '' then
							kind.menu = ''
						else
							kind.menu = '    (' .. (strings[2] or '') .. ')'
						end
						return kind
					end,
				},
				experimental = {
					ghost_text = {
						hl_group = 'LspCodeLens',
					},
				},
				confirm_opts = {
					behavior = cmp.ConfirmBehavior.Replace,
					select = false,
				},
			}

			local completion_mapping = cmp.mapping.preset.cmdline {
				['<C-l>'] = {
					-- select grabs first item if none were selected
					c = function() cmp.confirm { select = true } end,
				},
				['<C-j>'] = {
					c = function() cmp.select_next_item() end,
				},
				['<C-k>'] = {
					c = function() cmp.select_prev_item() end,
				},
			}

			cmp.setup.cmdline(':', {
				mapping = completion_mapping,
				sources = cmp.config.sources({
					{ name = 'path' },
				}, {
					{ name = 'cmdline' },
				}),
				completion = {
					keyword_length = 1,
				},
				confirm_opts = {
					behavior = cmp.ConfirmBehavior.Replace,
					select = true,
				},
			})

			cmp.setup.cmdline({ '/', '?' }, {
				mapping = completion_mapping,
				sources = {
					{ name = 'buffer' },
				},
				completion = {
					keyword_length = 1,
				},
			})

			local function complete(sources)
				local names = type(sources) == 'string' and { sources } or sources
				return function()
					cmp.complete {
						config = {
							sources = vim.tbl_map(function(n) return { name = n } end, names),
						},
					}
				end
			end

			U.keymap('i', '<C-Space>', complete { 'nvim_lsp' }, 'Cmp')

			U.keymap('i', '<C-x><C-p>', complete 'luasnip')

			U.keymap('i', '<C-x><C-f>', complete 'path')

			U.keymap('i', '<C-x><C-b>', function()
				cmp.complete {
					config = {
						sources = {
							{
								name = 'buffer',
								option = {
									get_bufnrs = function()
										-- source from all visible buffers
										-- TODO could experiment with last 5 buffers too
										local bufs = {}
										for _, win in ipairs(vim.api.nvim_list_wins()) do
											bufs[vim.api.nvim_win_get_buf(win)] = true
										end
										return vim.tbl_keys(bufs)
									end,
								},
							},
						},
					},
				}
			end, 'Buffer completion')
		end,
	},
}
