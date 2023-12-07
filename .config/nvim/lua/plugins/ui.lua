vim.api.nvim_create_user_command('DebugSpacing', function()
	vim.cmd 'set list'
	vim.cmd 'IndentBlanklineToggle'
	vim.b.miniindentscope_disable = true
end, {})

return {
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
		priority = 1000,
		lazy = false,
		opts = {
			variant = 'moon',
			-- calls to nvim_set_hl()
			highlight_groups = {
				NonText = { fg = 'base' }, -- end ~
				ColorColumn = { bg = 'rose' },
				-- Blend colours against the "base" background
				CursorLine = { bg = 'foam', blend = 10 },
				StatusLine = { fg = 'foam', bg = 'foam', blend = 10 },

				['@keyword.bang'] = { fg = 'love', underline = true },
				['@keyword.return'] = { fg = 'iris', undercurl = true },

				-- js stuff
				-- ['@arrow_function.const'] = { undercurl = true },
				['@keyword.export'] = { fg = 'love' },
				['@keyword.default'] = { fg = 'love', bold = true },
				-- ['@variable'] = { fg = c.fg }
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
