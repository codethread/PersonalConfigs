local u = require 'codethread.utils'
local nmap = u.nmap
local command = u.command

local M = {}

-- M.lsp_progress = function(client) vim.notify('lsp started: ' .. client.name) end
M.lsp_progress = function(client) end

M.lsp_highlight_document = function(client)
	-- Set autocommands conditional on server_capabilities
	if client.resolved_capabilities.document_highlight then
		vim.api.nvim_exec(
			[[
      augroup lsp_document_highlight
        autocmd! * <buffer>
        autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
        autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
      augroup END
    ]],
			false
		)
	end
end

M.lsp_keymaps = function()
	nmap('gD', function() vim.lsp.buf.declaration() end)
	nmap('gd', function() vim.lsp.buf.definition() end)
	nmap('K', function() vim.lsp.buf.hover() end)
	nmap('gi', function() vim.lsp.buf.implementation() end)
	nmap('gh', function() vim.lsp.buf.implementation() end)
	nmap('gr', function() vim.lsp.buf.references() end)

	command('format the buffer with LSP', 'Format', function() vim.lsp.buf.formatting() end)
end

return M
