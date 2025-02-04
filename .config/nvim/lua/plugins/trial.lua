return {
	'ThePrimeagen/vim-be-good',
	-- use { 'bennypowers/nvim-regexplainer', requires = 'MunifTanjim/nui.nvim' }
	{
		'OXY2DEV/markview.nvim',
		-- lazy = false, -- Recommended
		ft = 'markdown',
		enabled = true, -- this is sexy, play with later
		-- ft = "markdown" -- If you decide to lazy-load anyway
		config = function()
			local presets = require 'markview.presets'
			local mm = require 'markview'
			mm.setup {
				hybrid_modes = { 'i' }, -- Uses this feature on normal mode
				-- This is nice to have
				callbacks = {
					on_enable = function(_, win)
						vim.wo[win].conceallevel = 2
						vim.wo[win].concealcursor = 'nc'
					end,
				},

				-- ui
				code_blocks = {
					icons = 'devicons',
					style = 'simple',
					-- pad_amount = 3,
				},

				list_items = {
					marker_minus = {
						text = '•',
					},
					marker_plus = {
						text = '',
					},
					marker_star = {
						text = '',
					},
				},
			}
		end,
	},

	{
		'jmbuhr/otter.nvim',
		dependencies = {
			'nvim-treesitter/nvim-treesitter',
		},
		lazy = true,
		opts = {},
	},
	{
		'mistricky/codesnap.nvim',
		build = 'make',
		cmd = 'CodeSnap',
		opts = {
			min_width = 80,
			has_breadcrumbs = true,
			bg_x_padding = 61,
			bg_y_padding = 41,
			bg_theme = 'dusk',
		},
	},
}
