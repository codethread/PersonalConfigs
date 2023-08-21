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
				['@arrow_function.const'] = { underline = true },
				['@keyword.export'] = { fg = 'love' },
				['@keyword.default'] = { fg = 'love', bold = true },
				-- ['@variable'] = { fg = c.fg }
			},
		},
	},

	{
		'kevinhwang91/nvim-ufo',
		dependencies = { 'kevinhwang91/promise-async', 'nvim-treesitter/nvim-treesitter' },
		lazy = false,
		-- event = { 'BufReadPre', 'BufNewFile' },
		version = 'v1.*',
		init = function()
			vim.o.foldlevel = 99 -- Using ufo provider need a large value, feel free to decrease the value
			vim.o.foldlevelstart = 99
			vim.o.foldenable = true

			-- hide foldcolumn
			vim.o.foldcolumn = '0'
			-- or show with
			-- vim.o.foldcolumn = '1'
			-- vim.o.fillchars = [[eob: ,fold: ,foldopen:,foldsep: ,foldclose:]] -- add nice fold icons
		end,
		keys = {
			{ 'zR', function() require('ufo').openAllFolds() end, desc = 'open all folds' },
			{
				'zM',
				function() require('ufo').closeAllFolds() end,
				desc = 'close all folds',
			},
			{
				'zp',
				function() require('ufo').peekFoldedLinesUnderCursor() end,
				desc = 'peak lines',
			},
			{
				'-',
				'zc',
				desc = 'open fold under cursor',
			},
			{
				'=',
				'zo',
				desc = 'close fold under cursor',
			},
			{
				'_',
				'zC',
				desc = 'close all folds under cursor',
			},
			{
				'+',
				'zO',
				desc = 'open all folds under cursor',
			},
		},
		opts = {
			provider_selector = function() return { 'treesitter', 'indent' } end,
			fold_virt_text_handler = function(virtText, lnum, endLnum, width, truncate)
				local newVirtText = {}
				local suffix = ('  %d '):format(endLnum - lnum)
				local sufWidth = vim.fn.strdisplaywidth(suffix)
				local targetWidth = width - sufWidth
				local curWidth = 0
				for _, chunk in ipairs(virtText) do
					local chunkText = chunk[1]
					local chunkWidth = vim.fn.strdisplaywidth(chunkText)
					if targetWidth > curWidth + chunkWidth then
						table.insert(newVirtText, chunk)
					else
						chunkText = truncate(chunkText, targetWidth - curWidth)
						local hlGroup = chunk[2]
						table.insert(newVirtText, { chunkText, hlGroup })
						chunkWidth = vim.fn.strdisplaywidth(chunkText)
						-- str width returned from truncate() may less than 2nd argument, need padding
						if curWidth + chunkWidth < targetWidth then
							suffix = suffix .. (' '):rep(targetWidth - curWidth - chunkWidth)
						end
						break
					end
					curWidth = curWidth + chunkWidth
				end
				table.insert(newVirtText, { suffix, 'MoreMsg' })
				return newVirtText
			end,
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
