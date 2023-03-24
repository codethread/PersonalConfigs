local cmp_ok, cmp_nvim_lsp = pcall(require, 'cmp_nvim_lsp')
if not cmp_ok then
	print 'lsp cmp failed to load'
	return
end
local lspconfig_status_ok, lspconfig = pcall(require, 'lspconfig')
if not lspconfig_status_ok then
	print 'could not load lspconfig'
	return
end

-- Add additional capabilities supported by nvim-cmp
-- local capabilities = vim.lsp.protocol.make_client_capabilities()
local capabilities = cmp_nvim_lsp.default_capabilities()

-- handlers
-- https://github.com/neovim/nvim-lspconfig/wiki/UI-Customization
vim.lsp.handlers['textDocument/hover'] = vim.lsp.with(vim.lsp.handlers.hover, {
	border = 'double',
})

vim.lsp.handlers['textDocument/signatureHelp'] = vim.lsp.with(vim.lsp.handlers.signature_help, {
	border = 'rounded',
})

local servers = {
	'bashls',
	'clangd',
	'cssls',
	'eslint',
	'gopls',
	'html',
	'jsonls',
	'rust_analyzer',
	'lua_ls',
	'svelte',
	'tailwindcss',
}

require('mason-lspconfig').setup {
	ensure_installed = servers,
}

for _, lsp in ipairs(servers) do
	local config_ok, config_opts = pcall(require, 'codethread.lsp.settings.' .. lsp)

	config_opts.capabilities = capabilities

	if config_ok then
		lspconfig[lsp].setup(config_opts)
	else
		print('no configs for server ' .. lsp)
	end
end

require 'codethread.lsp.settings.typescript'
require 'codethread.lsp.settings.haskell'

U.telescope_hook 'lsp_handlers'
