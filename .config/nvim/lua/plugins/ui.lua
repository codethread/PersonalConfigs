vim.api.nvim_create_user_command('DebugSpacing', function()
	vim.cmd 'set list'
	vim.cmd 'IndentBlanklineToggle'
	vim.b.miniindentscope_disable = true
end, {})

return {

	{
		'folke/todo-comments.nvim',
		event = 'VimEnter',
		dependencies = { 'nvim-lua/plenary.nvim' },
		opts = {
			signs = false,

			search = {
				args = {
					'--color=never',
					'--no-heading',
					'--with-filename',
					'--line-number',
					'--column',
					'--hidden',
				},
			},
		},
	},

	{
		'lukas-reineke/indent-blankline.nvim',
		event = { 'BufReadPost', 'BufNewFile' },
		version = 'v2.*',
		dependencies = {
			U.highlights {
				IndentBlanklineChar = { fg = 'overlay' },
			},
		},
		opts = {
			char = '┊',
			-- char = '│',
			show_current_context = false, -- off because we are using mini.indentscop
			show_current_context_start = false,
			show_trailing_blankline_indent = false,
			filetype_exclude = {
				'help',
				'alpha',
				'dashboard',
				'neo-tree',
				'Trouble',
				'lazy',
				'mason',
				'oil',
			},
		},
	},

	{
		'echasnovski/mini.indentscope',
		version = false, -- wait till new 0.7.0 release to put it back on semver
		event = { 'BufReadPre', 'BufNewFile' },
		dependencies = {
			U.highlights {
				MiniIndentscopeSymbol = { fg = 'iris' },
			},
		},
		opts = {
			symbol = '│',
			options = { try_as_border = true },
		},
	},

	{ 'norcalli/nvim-colorizer.lua', cmd = 'ColorizerToggle' },
	{
		'rose-pine/neovim',
		name = 'rose-pine',
		version = 'v3.*',
		priority = 1000,
		lazy = false,
		opts = {
			variant = 'moon',
			styles = { italic = false, transparency = false },
			enable = {
				terminal = true,
				-- Improve compatibility for previous versions of Neovim
				legacy_highlights = false,
				-- Handle deprecated options automatically
				migrations = false,
			},
			-- calls to nvim_set_hl()
			dim_inactive_windows = true,
			extend_background_behind_borders = true,
			highlight_groups = {
				NonText = { fg = 'base' }, -- end ~
				ColorColumn = { bg = 'rose' },
				-- Blend colours against the "base" background
				CursorLine = { bg = 'foam', blend = 10 },
				StatusLine = { fg = 'foam', bg = 'foam', blend = 10 },

				['@variable'] = { italic = false },

				['@keyword.bang'] = { fg = 'love', underline = true },
				['@keyword.return'] = { fg = 'iris', undercurl = true },

				-- js stuff
				-- ['@arrow_function.const'] = { undercurl = true },
				['@keyword.export'] = { fg = 'love' },
				['@keyword.default'] = { fg = 'love', bold = true },

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
		},
	},

	{
		'goolord/alpha-nvim',
		enabled = false, -- charming, but undermines my lazy loading efforts
		dependencies = { 'nvim-tree/nvim-web-devicons' },
		opts = function()
			local config = require('alpha.themes.startify').config
			return config
		end,
	},

	{
		'stevearc/dressing.nvim',
		opts = {},
	},

	{
		'folke/noice.nvim',
		enabled = false,
		event = 'VeryLazy',
		setup = function(opts) require('noice').setup(opts) end,
		---@type NoiceConfig
		opts = {
			lsp = {
				-- override markdown rendering so that **cmp** and other plugins use **Treesitter**
				override = {
					['vim.lsp.util.convert_input_to_markdown_lines'] = true,
					['vim.lsp.util.stylize_markdown'] = true,
					['cmp.entry.get_documentation'] = true, -- requires hrsh7th/nvim-cmp
				},
			},
			-- you can enable a preset for easier configuration
			presets = {
				bottom_search = true, -- use a classic bottom cmdline for search
				command_palette = true, -- position the cmdline and popupmenu together
				long_message_to_split = true, -- long messages will be sent to a split
				inc_rename = false, -- enables an input dialog for inc-rename.nvim
				lsp_doc_border = false, -- add a border to hover docs and signature help
			},
		},
		dependencies = {
			-- if you lazy-load any plugin below, make sure to add proper `module="..."` entries
			'MunifTanjim/nui.nvim',
		},
	},

	{
		'nvim-neo-tree/neo-tree.nvim',
		branch = 'v3.x',
		dependencies = {
			'nvim-lua/plenary.nvim',
			'nvim-tree/nvim-web-devicons', -- not strictly required, but recommended
			'MunifTanjim/nui.nvim',
			-- "3rd/image.nvim", -- Optional image support in preview window: See `# Preview Mode` for more information
		},
		cmd = 'Neotree',
		setup = function()
			require('neo-tree').setup {
				window = {
					mappings = {
						['Z'] = 'expand_all_nodes',
					},
				},
			}
		end,
	},

	{
		'DanilaMihailov/beacon.nvim',
		enabled = false,
	},

	-- {
	-- 	'sphamba/smear-cursor.nvim',
	-- 	opts = {},
	-- },
}
