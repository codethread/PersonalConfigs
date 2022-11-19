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

vim.api.nvim_create_autocmd('BufEnter', {
	desc = "Easy quit help with 'q'",
	group = vim.api.nvim_create_augroup('Helpful', { clear = true }),
	-- pattern = '$VIMRUNTIME/doc/*.txt',
	pattern = '*.txt',
	callback = function()
		if vim.bo.filetype == 'help' then
			vim.keymap.set('n', 'q', '<cmd>q<cr>', { silent = true, buffer = true })
		end
	end,
})
