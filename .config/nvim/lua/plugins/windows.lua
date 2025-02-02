return {
	{ 'xorid/swap-split.nvim', cmd = 'SwapSplit' },

	{ 'shortcuts/no-neck-pain.nvim', cmd = { 'NoNeckPain' } },

	{ 'declancm/maximize.nvim', opts = {}, cmd = { 'Maximize' } },

	-- TODO: why do i have three things here
	{
		'christoomey/vim-tmux-navigator',
		enabled = false,
		init = function()
			vim.cmd [[
			" nnoremap <silent> <C-h> :wincmd h<CR>
			" nnoremap <silent> <C-j> :wincmd j<CR>
			" nnoremap <silent> <C-k> :wincmd k<CR>
			" nnoremap <silent> <C-l> :wincmd l<CR>

			" Disable tmux navigator when zooming the Vim pane
			let g:tmux_navigator_disable_when_zoomed = 1
			let g:tmux_navigator_no_mappings = 1
			nnoremap <silent> <C-h> :TmuxNavigateLeft<cr>
			nnoremap <silent> <C-j> :TmuxNavigateDown<cr>
			nnoremap <silent> <C-k> :TmuxNavigateUp<cr>
			nnoremap <silent> <C-l> :TmuxNavigateRight<cr>
			]]
		end,
	},

	{
		'mrjones2014/smart-splits.nvim',
		lazy = false,
		init = function()
			-- recommended mappings
			-- resizing splits
			-- these keymaps will also accept a range,
			-- for example `10<A-h>` will `resize_left` by `(10 * config.default_amount)`
			-- vim.keymap.set('n', '<A-h>', require('smart-splits').resize_left)
			-- vim.keymap.set('n', '<A-j>', require('smart-splits').resize_down)
			-- vim.keymap.set('n', '<A-k>', require('smart-splits').resize_up)
			-- vim.keymap.set('n', '<A-l>', require('smart-splits').resize_right)
			-- moving between splits
			vim.keymap.set('n', '<C-h>', require('smart-splits').move_cursor_left)
			vim.keymap.set('n', '<C-j>', require('smart-splits').move_cursor_down)
			vim.keymap.set('n', '<C-k>', require('smart-splits').move_cursor_up)
			vim.keymap.set('n', '<C-l>', require('smart-splits').move_cursor_right)
			vim.keymap.set('n', '<C-\\>', require('smart-splits').move_cursor_previous)
			-- swapping buffers between windows
			-- vim.keymap.set('n', '<leader>,h', require('smart-splits').swap_buf_left)
			-- vim.keymap.set('n', '<leader>,j', require('smart-splits').swap_buf_down)
			-- vim.keymap.set('n', '<leader>,k', require('smart-splits').swap_buf_up)
			-- vim.keymap.set('n', '<leader>,l', require('smart-splits').swap_buf_right)
		end,
		opts = {
			log_level = 'warn',
		},
	},
	{
		'alexghergh/nvim-tmux-navigation',
		enabled = false,
		config = function()
			require('nvim-tmux-navigation').setup {
				disable_when_zoomed = true, -- defaults to false
				keybindings = {
					left = '<C-h>',
					down = '<C-j>',
					up = '<C-k>',
					right = '<C-l>',
					last_active = '<C-\\>',
					-- next = '<C-Space>',
				},
			}
		end,
	},
}
