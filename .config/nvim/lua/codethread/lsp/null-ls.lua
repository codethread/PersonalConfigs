local null_ls_status_ok, null_ls = pcall(require, "null-ls")

if not null_ls_status_ok then
	print("could not load null ls")
	return
end

-- https://github.com/jose-elias-alvarez/null-ls.nvim/tree/main/lua/null-ls/builtins/formatting
local formatting = null_ls.builtins.formatting
-- https://github.com/jose-elias-alvarez/null-ls.nvim/tree/main/lua/null-ls/builtins/diagnostics
local diagnostics = null_ls.builtins.diagnostics
local code_actions = null_ls.builtins.code_actions

null_ls.setup({
	debug = false,
	sources = {
		-- lua
		formatting.stylua,
		-- rust
		formatting.rustfmt,
		-- js/ts
		formatting.prettierd,
		diagnostics.eslint_d,
		code_actions.eslint_d,
	},
})

vim.cmd([[
  augroup _lsp
    autocmd!
    autocmd BufWritePre * lua vim.lsp.buf.formatting_sync()
  augroup end
]])
