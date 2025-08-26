if vim.g.vscode then return {} end

return {
	{ 'xorid/swap-split.nvim', cmd = 'SwapSplit' },

	{ 'shortcuts/no-neck-pain.nvim', cmd = { 'NoNeckPain' } },

	{ 'declancm/maximize.nvim', opts = {}, cmd = { 'Maximize' } },

	{
		'mrjones2014/smart-splits.nvim',
		build = './kitty/install-kittens.bash',
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
			-- disable_multiplexer_nav_when_zoomed = true,
		},
	},
}
