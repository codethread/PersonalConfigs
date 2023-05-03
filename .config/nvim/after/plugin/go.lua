local require = require('codethread.utils').require
local go, ok = require 'go'
if not ok then return end

go.setup {
	dap_debug_keymap = false,
}

local format_sync_grp = vim.api.nvim_create_augroup('GoFormat', {})
vim.api.nvim_create_autocmd('BufWritePre', {
	pattern = '*.go',
	callback = function() require('go.format').goimport() end,
	group = format_sync_grp,
})

-- TODO make safe
local Term = require('toggleterm.terminal').Terminal

local go_run =
	Term:new { cmd = 'go run .', hidden = true, close_on_exit = false, direction = 'vertical' }

local cmd = U.cmd

U.wk('go', {
	r = {
		function()
			vim.cmd.wa()
			go_run:toggle()
		end,
		'GoRun',
	},
	e = { cmd 'GoIfErr', 'err' },
})
