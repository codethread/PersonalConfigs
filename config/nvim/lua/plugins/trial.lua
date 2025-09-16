return {
	-- 'ThePrimeagen/vim-be-good',
	-- use { 'bennypowers/nvim-regexplainer', requires = 'MunifTanjim/nui.nvim' }

	{
		-- lsp features and a code completion source for code embedded in other documents
		'jmbuhr/otter.nvim',
		dependencies = {
			'nvim-treesitter/nvim-treesitter',
		},
		cond = not vim.g.vscode,
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
