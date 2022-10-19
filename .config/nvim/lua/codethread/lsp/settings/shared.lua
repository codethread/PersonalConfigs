local u = require 'codethread.utils'
local nmap = u.nmap
local command = u.command

local nvim_navic_status_ok, nvim_navic = pcall(require, 'nvim-navic')

local M = {}

local augroup = vim.api.nvim_create_augroup('LspFormatting', {})

function M.on_attach(client, bufnr)
	-- get capabilities
	-- lua =vim.lsp.get_active_clients()[1].server_capabilities

	if client.supports_method 'textDocument/formatting' then
		-- TODO turn off for *keymap.c
		vim.api.nvim_clear_autocmds { group = augroup, buffer = bufnr }
		vim.api.nvim_create_autocmd('BufWritePre', {
			group = augroup,
			buffer = bufnr,
			callback = function()
				vim.lsp.buf.format {
					-- this is straight from the docs and needs refining for servers i actually want to use
					filter = function(c) return c.name == 'null-ls' end,
					bufnr = bufnr,
				}
			end,
		})
	end

	if client.server_capabilities.documentSymbolProvider then
		if nvim_navic_status_ok then nvim_navic.attach(client, bufnr) end
	end

	if client.server_capabilities.documentHighlightProvider then
		vim.cmd [[
      augroup lsp_document_highlight
        autocmd! * <buffer>
        autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
        autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
      augroup END
		]]
	end

	nmap('gD', function() vim.lsp.buf.declaration() end)
	nmap('gd', function() vim.lsp.buf.definition() end)
	nmap('K', function() vim.lsp.buf.hover() end)
	nmap('gi', function() vim.lsp.buf.implementation() end)
	nmap('gh', function() vim.lsp.buf.signature_help() end)
	nmap('gr', function() vim.lsp.buf.references() end)

	command('format the buffer with LSP', 'Format', function() vim.lsp.buf.formatting() end)
end

return M
