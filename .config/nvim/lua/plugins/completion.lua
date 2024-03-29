return {
	-- snippets
	{
		'L3MON4D3/LuaSnip',
		version = 'v2.*',
		dependencies = 'rafamadriz/friendly-snippets',
		config = function()
			require('luasnip').setup {
				history = true,
				delete_check_events = 'TextChanged',
			}
			require('luasnip.loaders.from_vscode').lazy_load {
				paths = {
					'~/.config/nvim/snippets_vscode',
					'~/.local/share/nvim/lazy/friendly-snippets',
				},
			}
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
					Copilot = '',
					TypeParameter = '',
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
			local ls = require 'luasnip'
			cmp.setup {
				-- performance.max_view_entries
				window = {
					completion = {
						winhighlight = 'Normal:Pmenu,FloatBorder:Pmenu,Search:None',
						col_offset = -3,
						side_padding = 0,
					},
					documentation = cmp.config.window.bordered(),
				},
				snippet = {
					expand = function(args) require('luasnip').lsp_expand(args.body) end,
				},
				mapping = {
					['<C-k>'] = cmp.mapping.select_prev_item(),
					['<C-j>'] = cmp.mapping.select_next_item(),
					['<C-l>'] = cmp.mapping.confirm { select = true }, -- select grabs first item if none were selected
					['<Esc>'] = cmp.mapping.abort(),
					['<C-c>'] = cmp.mapping.abort(),
					-- ['<C-Space>'] = cmp.mapping.complete_common_string(),
					['<C-u>'] = cmp.mapping.scroll_docs(-4),
					['<C-d>'] = cmp.mapping.scroll_docs(4),

					['<Tab>'] = cmp.mapping(function(fallback)
						if ls.expandable() then
							ls.expand()
						elseif ls.expand_or_jumpable() then
							ls.expand_or_jump()
						else
							fallback()
						end
					end, {
						'i',
						's',
					}),
					['<S-Tab>'] = cmp.mapping(function(fallback)
						if cmp.visible() then
							cmp.select_prev_item()
						elseif ls.jumpable(-1) then
							ls.jump(-1)
						else
							fallback()
						end
					end, {
						'i',
						's',
					}),
				},
				completion = {
					completeopt = 'menu,menuone,noinsert',
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
						local a = strings[1] or 'OOF'
						local b = strings[2] or 'YEP'
						kind.kind = ' ' .. a .. ' '
						kind.menu = '    (' .. b .. ')'

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
			cmp.setup.cmdline(':', {
				mapping = cmp.mapping.preset.cmdline {
					['<C-l>'] = {
						c = function() cmp.complete() end,
					},
					['<C-j>'] = {
						c = function()
							if cmp.visible() then
								cmp.select_next_item()
							else
								cmp.complete()
							end
						end,
					},
					['<C-k>'] = {
						c = function()
							if cmp.visible() then
								cmp.select_prev_item()
							else
								cmp.complete()
							end
						end,
					},
				},
				sources = cmp.config.sources({
					{ name = 'path' },
				}, {
					{ name = 'cmdline' },
				}),
				completion = {
					-- completeopt = 'menu,menuone,noselect',
					keyword_length = 1,
				},
				confirm_opts = {
					behavior = cmp.ConfirmBehavior.Replace,
					select = true,
				},
			})

			cmp.setup.cmdline({ '/', '?' }, {
				mapping = cmp.mapping.preset.cmdline(),
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

			U.keymap('i', '<C-Space>', complete { 'nvim_lsp', 'copilot' }, 'Cmp')

			U.keymap('i', '<C-p>', complete 'luasnip')
			U.keymap('i', '<C-x><C-f>', complete 'path')

			U.keymap('i', '<C-n>', function()
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
