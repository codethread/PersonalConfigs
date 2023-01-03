vim.cmd [[
" only show cursor for active window
augroup CursorLine
  au!
  au WinEnter,BufWinEnter * setlocal cursorline
  au WinLeave * setlocal nocursorline
  au FileType TelescopePrompt setlocal nocursorline
augroup END

augroup JSONChange
  au!
  au BufRead,BufNewFile *.json set filetype=jsonc
augroup END
]]

vim.api.nvim_create_autocmd('TextYankPost', {
	desc = 'Highlight on yank',
	callback = function()
		vim.highlight.on_yank {
			higroup = 'WildMenu',
			on_macro = true,
		}
	end,
	group = vim.api.nvim_create_augroup('YankHighlight', { clear = true }),
	pattern = '*',
})

vim.api.nvim_create_autocmd('FileType', {
	desc = "Easy quit help with 'q'",
	group = vim.api.nvim_create_augroup('Helpful', { clear = true }),
	pattern = 'help',
	callback = function() vim.keymap.set('n', 'q', '<cmd>q<cr>', { silent = true, buffer = true }) end,
})

vim.api.nvim_create_autocmd('BufReadPost', {
	desc = 'Open old buffers at last position',
	callback = function()
		local mark = vim.api.nvim_buf_get_mark(0, '"')
		local lcount = vim.api.nvim_buf_line_count(0)
		if mark[1] > 0 and mark[1] <= lcount then pcall(vim.api.nvim_win_set_cursor, 0, mark) end
	end,
})
