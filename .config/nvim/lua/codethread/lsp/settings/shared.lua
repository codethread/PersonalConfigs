local u = require 'codethread.utils'
local nmap_buf = function(lhs, rhs, opts)
	u.nmap(lhs, rhs, vim.tbl_extend('force', { buffer = 0 }, opts or {}))
end

local command = u.command

local M = {}

local augroup = vim.api.nvim_create_augroup('LspFormatting', {})
local higroup = vim.api.nvim_create_augroup('LspHighlight', {})

local function highlight_symbol(opts)
	-- not sure how to negate a pattern in autocmd, so turning off for lua
	if vim.endswith(opts.file, '.lua') then
		-- check not in a string, which annoys me (or block comment with nested syntax)
		-- there might be a better way, but this returns a list starting with the outer scope
		-- moving down
		if vim.treesitter.get_captures_at_cursor(0)[1] ~= 'string' then
			vim.lsp.buf.document_highlight()
		end
	else
		vim.lsp.buf.document_highlight()
	end
end

function M.on_attach(client, bufnr)
	-- get capabilities
	-- lua =vim.lsp.get_active_clients()[1].server_capabilities

	if client.supports_method 'textDocument/formatting' then
		u.autocmd('BufWritePre', {
			group = augroup,
			buffer = bufnr,
			fn = function(opts)
				if vim.endswith(opts.file, 'keymap.c') then return end
				vim.lsp.buf.format {
					-- this is straight from the docs and needs refining for servers i actually want to use
					filter = function(c) return c.name == 'null-ls' end,
					bufnr = bufnr,
				}
			end,
		})
	end

	if client.server_capabilities.documentSymbolProvider then
		require('codethread.utils').safe_load(
			'nvim-navic',
			function(nvim_navic) nvim_navic.attach(client, bufnr) end
		)
	end

	if client.server_capabilities.documentHighlightProvider then
		vim.api.nvim_clear_autocmds { group = higroup, buffer = bufnr }
		vim.api.nvim_create_autocmd(
			'CursorHold',
			{ group = higroup, buffer = bufnr, callback = highlight_symbol }
		)
		vim.api.nvim_create_autocmd('CursorMoved', {
			group = higroup,
			buffer = bufnr,
			callback = function() vim.lsp.buf.clear_references() end,
		})
	end

	nmap_buf('gD', function() vim.lsp.buf.declaration() end)
	nmap_buf('gd', function() vim.lsp.buf.definition() end)
	nmap_buf('K', function() vim.lsp.buf.hover() end, { desc = 'lsp hover' })
	nmap_buf('gi', function() vim.lsp.buf.implementation() end)
	nmap_buf('gh', function() vim.lsp.buf.signature_help() end)
	nmap_buf('gr', function() vim.lsp.buf.references() end)

	command('format the buffer with LSP', 'Format', function() vim.lsp.buf.formatting() end)
end

return M
