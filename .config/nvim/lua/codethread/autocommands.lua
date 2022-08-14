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

-- wip
vim.api.nvim_create_autocmd('BufWritePost', {
	callback = function(opts)
		-- {
		--  buf = 1,
		--  event = "BufWritePost",
		--  file = ".config/nvim/init.lua",
		--  group = 11,
		--  id = 2,
		--  match = "/Users/adam/PersonalConfigs/.config/nvim/init.lua"
		-- }
		--
		-- don't want this to run for
		--   buf = 84,
		--event = "BufWritePost",
		--file = ".git/NEOGIT_COMMIT_EDITMSG",
		--group = 11,
		--id = 2,
		--match = "/Users/adam/PersonalConfigs/.git/NEOGIT_COMMIT_EDITMSG"

		print 'bufwrite'
		print(vim.inspect(opts))
	end,
	group = vim.api.nvim_create_augroup('Dotty', { clear = true }),
	pattern = '*/PersonalConfigs/*',
})
