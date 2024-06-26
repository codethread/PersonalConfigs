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

return {
	'wellle/targets.vim',

	{ 'tpope/vim-rsi', event = { 'InsertEnter', 'CmdlineEnter' } }, -- readline movement, e.g C-f is forward char

	{
		'kylechui/nvim-surround',
		version = 'v1.*',
		event = 'VeryLazy',
		config = true,
	},

	{
		-- Text editing in Neovim with immediate visual feedback: view the effects of any
		-- command on your buffer contents live. Preview macros, the :norm command & more!
		'smjonas/live-command.nvim',
		main = 'live-command',
		opts = {
			defaults = {
				-- inline_highlighting = false,
			},
			-- break_undo = false,
			commands = {
				Norm = {
					-- :%Norm 0f{ciwlook mum, no hands!
					cmd = 'norm',
				},
				Reg = {
					-- This will transform ":5Reg a" into ":norm 5@a", running a captured macro with a preview
					cmd = 'norm',
					args = function(opts)
						return (opts.count == -1 and '' or opts.count) .. '@' .. opts.args
					end,
					range = '',
				},
				S = {
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
}
