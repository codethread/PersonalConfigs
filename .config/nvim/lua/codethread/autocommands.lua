vim.cmd [[
" show cursor line for inactive buffer to make context switching easier
augroup CursorLine
  au!
  au WinEnter,BufWinEnter * setlocal nocursorline
  au WinLeave * setlocal cursorline
augroup END

augroup JSONChange
  au! BufRead,BufNewFile *.json set filetype=jsonc
augroup END
]]

-- Highlight on yank
vim.api.nvim_create_autocmd('TextYankPost', {
	callback = function()
		vim.highlight.on_yank {
			higroup = 'WildMenu',
			on_macro = true,
		}
	end,
	group = vim.api.nvim_create_augroup('YankHighlight', { clear = true }),
	pattern = '*',
})
