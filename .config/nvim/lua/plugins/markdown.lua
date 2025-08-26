if vim.g.vscode then return {} end

return U.F {
	{
		'mzlogin/vim-markdown-toc',
		init = function() vim.cmd [[let g:vmt_auto_update_on_save = 0]] end,
	},

	{
		'iamcco/markdown-preview.nvim',
		cmd = { 'MarkdownPreviewToggle', 'MarkdownPreview', 'MarkdownPreviewStop' },
		-- build = 'sh -c "cd app yarn install"',
		build = 'sh -c "cd app && yarn install"',
		init = function()
			-- vim.g.mkdp_browser = 'firefox'
			vim.g.mkdp_filetypes = { 'markdown' }
			vim.g.mkdp_refresh_slow = 1 -- too much jumping around, refresh on save or insert leave
		end,
		ft = { 'markdown' },
	},

	{
		'lukas-reineke/headlines.nvim',
		enabled = false,
		ft = 'markdown',
		dependencies = {
			'nvim-treesitter/nvim-treesitter',
			U.highlights {
				Headline = { bg = 'surface' },
				CodeBlock = { bg = 'surface' },
			},
		},
		config = false, -- or `opts = {}`
	},

	{
		'OXY2DEV/markview.nvim',
		-- lazy = false, -- Recommended
		ft = 'markdown',
		-- enabled = false, -- this is sexy, play with later
		-- ft = "markdown" -- If you decide to lazy-load anyway
		opts = {},
		-- config = function()
		-- 	local presets = require 'markview.presets'
		-- 	local mm = require 'markview'
		-- 	mm.setup {
		-- 		hybrid_modes = { 'i' }, -- Uses this feature on normal mode
		-- 		-- This is nice to have
		-- 		callbacks = {
		-- 			on_enable = function(_, win)
		-- 				vim.wo[win].conceallevel = 2
		-- 				vim.wo[win].concealcursor = 'nc'
		-- 			end,
		-- 		},
		--
		-- 		-- ui
		-- 		code_blocks = {
		-- 			icons = 'devicons',
		-- 			style = 'simple',
		-- 			-- pad_amount = 3,
		-- 		},
		--
		-- 		list_items = {
		-- 			marker_minus = {
		-- 				text = '•',
		-- 			},
		-- 			marker_plus = {
		-- 				text = '',
		-- 			},
		-- 			marker_star = {
		-- 				text = '',
		-- 			},
		-- 		},
		-- 	}
		-- end,
	},
}
