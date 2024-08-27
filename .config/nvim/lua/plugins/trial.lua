return {
	'ThePrimeagen/vim-be-good',
	-- use { 'bennypowers/nvim-regexplainer', requires = 'MunifTanjim/nui.nvim' }
	{
		'OXY2DEV/markview.nvim',
		lazy = false, -- Recommended
		enabled = false, -- this is sexy, play with later
		-- ft = "markdown" -- If you decide to lazy-load anyway
		config = function()
			require('markview').setup {
				modes = { 'n', 'i', 'no', 'c' }, -- Change these modes
				-- to what you need

				hybrid_modes = { 'i' }, -- Uses this feature on
				-- normal mode

				-- This is nice to have
				callbacks = {
					on_enable = function(_, win)
						vim.wo[win].conceallevel = 2
						vim.wo[win].concealcursor = 'nc'
					end,
				},
			}
		end,
	},
}
