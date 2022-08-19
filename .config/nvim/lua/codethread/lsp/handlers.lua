local M = {}

M.setup = function()
	-- https://github.com/neovim/nvim-lspconfig/wiki/UI-Customization
	vim.lsp.handlers['textDocument/hover'] = vim.lsp.with(vim.lsp.handlers.hover, {
		border = 'double',
	})

	vim.lsp.handlers['textDocument/signatureHelp'] = vim.lsp.with(vim.lsp.handlers.signature_help, {
		border = 'rounded',
	})
end

M.setup()

return M
