return U.F {
	{
		'yioneko/nvim-vtsls',
		ft = U.ecma_ft,
		lazy = true,
		config = function()
			-- Configure vtsls helper plugin
			require('vtsls').config({
				-- Automatically rename imports when moving files
				refactor_auto_rename = true,
			})
		end,
	},
}
