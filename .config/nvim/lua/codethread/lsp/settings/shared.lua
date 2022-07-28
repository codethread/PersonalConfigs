local command = require('codethread.utils').command

local function nmap(desc, lhs, rhs)
	vim.keymap.set('n', lhs, rhs, { desc = desc })
end

local M = {}

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
	nmap('go to declaration', 'gD', function()
		vim.lsp.buf.declaration()
	end)

	nmap('go to definition', 'gd', function()
		vim.lsp.buf.definition()
	end)

	nmap('hover lsp', 'K', function()
		vim.lsp.buf.hover()
	end)

	nmap('go to implementations', 'gi', function()
		vim.lsp.buf.implementation()
	end)

	nmap('hover signature help', 'gh', function()
		vim.lsp.buf.implementation()
	end)

	nmap('find references', 'gr', function()
		vim.lsp.buf.references()
	end)

	command('format the buffer with LSP', 'Format', function()
		vim.lsp.buf.formatting()
	end)
end

return M
