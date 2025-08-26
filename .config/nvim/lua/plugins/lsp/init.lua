if vim.g.vscode then return {} end

return U.flatten {
	require 'plugins.lsp.mason',
	require 'plugins.lsp.lspconfig',
	require 'plugins.lsp.linting',
}
