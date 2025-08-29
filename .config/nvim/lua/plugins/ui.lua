if vim.g.vscode then return {} end

return {
	{
		'lukas-reineke/indent-blankline.nvim',
		main = 'ibl',
		event = U.LazyFile,
		dependencies = {
			U.highlights {
				IblIndent = { fg = 'overlay' },
				IblScope = { fg = 'iris' },
			},
		},
		---@module "ibl"
		---@type ibl.config
		opts = {
			indent = {
				char = '┊',
			},
			scope = {
				enabled = true, --
				char = '│',
				show_start = false,
				show_end = false,
			},
		},
	},

	{
		'echasnovski/mini.indentscope',
		event = U.LazyFile,
		dependencies = {
			U.highlights {
				MiniIndentscopeSymbol = { fg = 'iris' },
			},
		},
		init = function()
			vim.api.nvim_create_autocmd(
				'BufNew',
				{ callback = function(opts) vim.b[opts.buf].miniindentscope_disable = true end }
			)
		end,
		opts = {
			symbol = '│',
			options = { try_as_border = true },
		},
	},

	{ 'norcalli/nvim-colorizer.lua', cmd = 'ColorizerToggle' },

	{
		'rose-pine/neovim',
		name = 'rose-pine',
		priority = 1000,
		lazy = false,
		config = function(_, opts)
			require('rose-pine').setup(vim.tbl_deep_extend('force', opts, {
				variant = 'moon',
				styles = { italic = true, transparency = true },
				enable = {
					terminal = true,
					-- Improve compatibility for previous versions of Neovim
					legacy_highlights = false,
					-- Handle deprecated options automatically
					migrations = false,
				},
				-- calls to nvim_set_hl()
				dim_inactive_windows = false,
				extend_background_behind_borders = true,
				---@type table<string, ct.RoseColor>
				highlight_groups = {
					NonText = { fg = 'base' }, -- end ~
					ColorColumn = { bg = 'rose' },
					-- Blend colours against the "base" background
					CursorLine = { bg = 'foam', blend = 10 },
					StatusLine = { fg = 'foam', bg = 'foam', blend = 10 },

					['@variable'] = { italic = false },
					['@variable.builtin'] = { fg = 'text', bold = true },

					['@keyword.bang'] = { fg = 'love', underline = true },
					['@keyword.return'] = { fg = 'iris' },

					-- js stuff
					-- ['@arrow_function.const'] = { undercurl = true },
					['@keyword.export'] = { fg = 'love' },
					['@keyword.default'] = { fg = 'love', bold = true },

					['@lsp.mod.async.typescript'] = { bold = true, undercurl = true },

					-- markdown
					-- @markup.italic.markdown_inline
					['@markup'] = { fg = 'rose' },
					['@markup.italic'] = { italic = true },
					['@markup.heading.1'] = { fg = 'gold', underline = true },
					['@markup.heading.2'] = { fg = 'rose', bold = true },

					-- italic, I prefer to do these myself and disable it globally
					['@text.emphasis'] = { italic = true },
					Comment = { italic = true },
					htmlItalic = { italic = true },
					mkdCode = { italic = true },
				},
			}))
			vim.cmd [[colorscheme rose-pine]]
		end,
	},

	{ -- just keep around for pairing
		'nvim-neo-tree/neo-tree.nvim',
		branch = 'v3.x',
		dependencies = { 'nvim-lua/plenary.nvim', 'MunifTanjim/nui.nvim' },
		cmd = 'Neotree',
		opts = { window = { mappings = { ['Z'] = 'expand_all_nodes' } } },
	},

	-- Keeping these around if moving to a termainl without builtin smear
	-- { 'DanilaMihailov/beacon.nvim' },
	-- { 'sphamba/smear-cursor.nvim', opts = {}, },
}
