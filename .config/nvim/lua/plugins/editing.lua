vim.cmd [[
	" paste in visual selection without adding to register
	xnoremap <leader>p "_dP

	" delete but without adding to register
	nnoremap x "_d
	nnoremap X "_D

	" Copy to clipboard
	vnoremap  <leader>y "+y
	nnoremap  <leader>Y "+yg_
	nnoremap  <leader>y "+y
	nnoremap  <leader>yy "+yy

	" Paste from clipboard
	nnoremap <leader>v "+p
	nnoremap <leader>v "+P
	vnoremap <leader>v "+p
	vnoremap <leader>v "+P

	" move text
	vnoremap <Down> :m '>+1<CR>gv=gv
	vnoremap <Up> :m '<-2<CR>gv=gv
]]

---@return LazySpec[]
return {
	'wellle/targets.vim',
	{ 'echasnovski/mini.align', opts = {} },
	{ 'echasnovski/mini.ai', opts = {} },

	{ 'tpope/vim-rsi', event = { 'InsertEnter', 'CmdlineEnter' } }, -- readline movement, e.g C-f is forward char

	{
		-- Text editing in Neovim with immediate visual feedback: view the effects of any
		-- command on your buffer contents live. Preview macros, the :norm command & more!
		'smjonas/live-command.nvim',
		main = 'live-command',
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
	'tpope/vim-abolish',
	-- also https://github.com/gregorias/coerce.nvim/tree/main if wanting lua api

	{
		'junegunn/vim-easy-align',
		init = function()
			-- TODO move
			vim.cmd [[
		" nmap ga <Plug>(EasyAlign)
		" xmap ga <Plug>(EasyAlign)
		]]
			vim.api.nvim_create_user_command(
				'CsvFormat',
				':EasyAlign *,<CR>',
				{ desc = 'Align a csv file' }
			)
		end,
	},

	{ 'xlboy/swap-ternary.nvim' }, -- seems over engineered but works

	{
		'folke/ts-comments.nvim',
		opts = {},
		event = 'VeryLazy',
	},

	-- printf style debugging
	-- normal mode g? , e.g g?p g?v
	-- visual mode g? , e.g g?v
	{ 'andrewferrier/debugprint.nvim', opts = {} },

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
