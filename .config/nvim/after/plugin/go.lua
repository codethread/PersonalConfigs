local require = require('codethread.utils').require
local go, ok = require 'go'
if not ok then return end

go.setup {}

local format_sync_grp = vim.api.nvim_create_augroup('GoFormat', {})
vim.api.nvim_create_autocmd('BufWritePre', {
	pattern = '*.go',
	callback = function() require('go.format').goimport() end,
	group = format_sync_grp,
})
