return U.F {
	{ 'echasnovski/mini.ai', event = U.LazyFile, opts = {} },

	{ 'tpope/vim-rsi', event = { 'InsertEnter', 'CmdlineEnter' } }, -- readline movement, e.g C-f is forward char

	{
		-- Text editing in Neovim with immediate visual feedback: view the effects of any
		-- command on your buffer contents live. Preview macros, the :norm command & more!
		'smjonas/live-command.nvim',
		main = 'live-command',
		event = 'CmdlineEnter',
		opts = {
			-- 	inline_highlighting = false,
			-- break_undo = false,
			commands = {
				Norm = {
					-- :%Norm 0f{ciwlook mum, no hands!
					cmd = 'norm',
				},
				S = {
					-- results
					-- run :%/result/outcome and see bottome text change, using vim-abolish
					-- :S/result{,s} can also be used as just a search
					cmd = 'Subvert',
				},
				G = {
					cmd = 'g',
				},
			},
		},
	},

	-- Want to turn fooBar into foo_bar? Press crs (coerce to snake_case). MixedCase
	-- (crm), camelCase (crc), snake_case (crs), UPPER_CASE (cru), dash-case (cr-),
	-- dot.case (cr.), space case (cr<space>), and Title Case (crt) are all just 3 keystrokes away.
	{ 'tpope/vim-abolish', event = U.LazyFile },
	-- also https://github.com/gregorias/coerce.nvim/tree/main if wanting lua api

	{ 'xlboy/swap-ternary.nvim', ft = U.ecma_ft }, -- seems over engineered but works

	{ 'folke/ts-comments.nvim', opts = {}, event = U.LazyFile },

	-- printf style debugging
	-- normal mode g? , e.g g?p g?v
	-- visual mode g? , e.g g?v
	{ 'andrewferrier/debugprint.nvim', event = { 'InsertEnter' }, opts = {} },

	{
		'ThePrimeagen/refactoring.nvim',
		dependencies = {
			'nvim-lua/plenary.nvim',
			'nvim-treesitter/nvim-treesitter',
		},
		lazy = true,
		init = function()
			-- stylua: ignore
			do
			vim.keymap.set("x", "<leader>re", function() require('refactoring').refactor('Extract Function') end)
			vim.keymap.set("x", "<leader>rf", function() require('refactoring').refactor('Extract Function To File') end)
			-- Extract function supports only visual mode
			vim.keymap.set("x", "<leader>rv", function() require('refactoring').refactor('Extract Variable') end)
			-- Extract variable supports only visual mode
			vim.keymap.set("n", "<leader>rI", function() require('refactoring').refactor('Inline Function') end)
			-- Inline func supports only normal
			vim.keymap.set({ "n", "x" }, "<leader>ri", function() require('refactoring').refactor('Inline Variable') end)
			-- Inline var supports both normal and visual mode

			vim.keymap.set("n", "<leader>rb", function() require('refactoring').refactor('Extract Block') end)
			vim.keymap.set("n", "<leader>rbf", function() require('refactoring').refactor('Extract Block To File') end)
			-- Extract block supports only normal mode
			end
		end,
		opts = {},
	},
}
