-- This file is automatically loaded by plugins.init

local function augroup(name)
	return vim.api.nvim_create_augroup('codethread_' .. name, { clear = true })
end

-- Check if we need to reload the file when it changed
vim.api.nvim_create_autocmd({ 'FocusGained', 'TermClose', 'TermLeave' }, {
	group = augroup 'checktime',
	command = 'checktime',
})

-- Highlight on yank
vim.api.nvim_create_autocmd('TextYankPost', {
	group = augroup 'highlight_yank',
	callback = function() vim.highlight.on_yank() end,
})

-- resize splits if window got resized
vim.api.nvim_create_autocmd({ 'VimResized' }, {
	group = augroup 'resize_splits',
	callback = function() vim.cmd 'tabdo wincmd =' end,
})

-- go to last loc when opening a buffer
vim.api.nvim_create_autocmd('BufReadPost', {
	group = augroup 'last_loc',
	callback = function()
		local mark = vim.api.nvim_buf_get_mark(0, '"')
		local lcount = vim.api.nvim_buf_line_count(0)
		if mark[1] > 0 and mark[1] <= lcount then pcall(vim.api.nvim_win_set_cursor, 0, mark) end
	end,
})

-- close some filetypes with <q>
vim.api.nvim_create_autocmd('FileType', {
	group = augroup 'close_with_q',
	pattern = {
		'PlenaryTestPopup',
		'help',
		'lspinfo',
		'man',
		'notify',
		'qf',
		'spectre_panel',
		'startuptime',
		'tsplayground',
		'checkhealth',
	},
	callback = function(event)
		vim.bo[event.buf].buflisted = false
		vim.keymap.set('n', 'q', '<cmd>close<cr>', { buffer = event.buf, silent = true })
	end,
})

-- wrap and check for spell in text filetypes
vim.api.nvim_create_autocmd('FileType', {
	group = augroup 'wrap_spell',
	pattern = { 'gitcommit', 'markdown' },
	callback = function()
		vim.opt_local.wrap = true
		vim.opt_local.spell = true
	end,
})

-- Auto create dir when saving a file, in case some intermediate directory does not exist
vim.api.nvim_create_autocmd({ 'BufWritePre' }, {
	group = augroup 'auto_create_dir',
	callback = function(event)
		if event.match:match '^%w%w+://' then return end
		local file = vim.loop.fs_realpath(event.match) or event.match
		vim.fn.mkdir(vim.fn.fnamemodify(file, ':p:h'), 'p')
	end,
})

-- local in_dotfiles =
-- 	vim.endswith(vim.fn.getcwd(), os.getenv 'DOTFILES' or os.getenv 'HOME' .. '/.config')
--
-- if in_dotfiles then
-- 	if vim.fn.executable 'dotty' then
-- 		---notify at info level
-- 		---@param msg string
-- 		local function dotty_info(msg)
--             vim.defer_fn(function()
-- 			vim.notify(msg, 1, {
-- 				title = 'Dotty',
-- 				hide_from_history = true,
-- 				timeout = 0,
-- 			})
--         end, 0)
-- 		end
--
-- 		vim.api.nvim_create_user_command('Dotty', function()
-- 			local Job = require 'plenary.job'
-- 			local dotty_setup_job = Job:new {
-- 				command = 'dotty',
-- 				args = { 'setup' },
-- 				on_start = function() dotty_info 'Running' end,
-- 			}
-- 			dotty_setup_job:start()
-- 		end, {})
--
-- 		vim.api.nvim_create_autocmd({ 'BufWritePost', 'BufFilePost', 'BufWipeout', 'VimLeavePre' }, {
-- 			desc = 'Run dotty when saving and deleting buffers',
-- 			group = vim.api.nvim_create_augroup('Dotty', {}),
-- 			pattern = os.getenv 'DOTFILES' .. '/*',
-- 			callback = function(opts)
-- 				if opts.file == nil then return end
-- 				local Job = require 'plenary.job'
-- 				local dotty_setup_job = Job:new {
-- 					command = 'dotty',
-- 					args = { 'setup' },
-- 					on_start = function() dotty_info 'Running' end,
-- 				}
--
-- 				local job = Job:new {
-- 					command = 'dotty',
-- 					args = { 'test', opts.file },
-- 				}
--
-- 				job:and_then_on_success(dotty_setup_job)
--
-- 				job:start()
-- 			end,
-- 		})
-- 	else
-- 		vim.notify('No Dotty bin, you may need to brew install', 'warn', {
-- 			title = 'Dotty',
-- 		})
-- 	end
-- end
