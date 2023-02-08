local ht = require 'haskell-tools'
local def_opts = { noremap = true, silent = true }
ht.setup {
	hls = {
		on_attach = function(client, bufnr)
			require('codethread.lsp.settings.shared').on_attach(client, bufnr)
			local opts = vim.tbl_extend('keep', def_opts, { buffer = bufnr })
			-- haskell-language-server relies heavily on codeLenses,
			-- so auto-refresh (see advanced configuration) is enabled by default
			vim.keymap.set('n', '<space>ca', vim.lsp.codelens.run, opts)
			vim.keymap.set('n', '<space>hs', ht.hoogle.hoogle_signature, opts)
			-- default_on_attach(client, bufnr)  -- if defined, see nvim-lspconfig
		end,
	},
}
-- Suggested keymaps that do not depend on haskell-language-server
-- Toggle a GHCi repl for the current package
vim.keymap.set('n', '<leader>rr', ht.repl.toggle, def_opts)
-- Toggle a GHCi repl for the current buffer
vim.keymap.set(
	'n',
	'<leader>rf',
	function() ht.repl.toggle(vim.api.nvim_buf_get_name(0)) end,
	def_opts
)
vim.keymap.set('n', '<leader>rq', ht.repl.quit, def_opts)
