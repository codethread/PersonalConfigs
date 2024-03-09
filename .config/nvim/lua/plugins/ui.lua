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
		opts = { signs = false },
	},

	{
		'rcarriga/nvim-notify',
		version = 'v3.*',
		config = function()
			local notify = require 'notify'

			notify.setup {
				-- background_colour = '#000000',
				max_width = 100,
				stages = 'slide',
				timeout = 500,
			}

			-- silence annoying errors from lsp's that have no hover information
			-- source https://github.com/neovim/nvim-lspconfig/issues/1931#issuecomment-1297599534
			local banned_messages = {
				'No information available',
				'warning: multiple different client offset_encodings detected for buffer, this is not supported yet',
			}

			---@diagnostic disable-next-line: duplicate-set-field
			vim.notify = function(msg, ...)
				for _, banned in ipairs(banned_messages) do
					if msg == banned then return end
				end

				notify(msg, ...)
			end
		end,
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
				['@markup.heading.1'] = { fg = 'rose', underline = true },
				['@markup.heading.2'] = { fg = 'gold', bold = true },

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
}
