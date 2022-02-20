return {
	settings = {
		Lua = {
			diagnostics = {
				globals = { "vim" },
			},
			workspace = {
				library = {
					[vim.fn.expand("$VIMRUNTIME/lua")] = true,
					[vim.fn.stdpath("config") .. "/lua"] = true,
				},
			},
		},
	},

	on_attach = function(client, bufnr)
		require("codethread.lsp.settings.shared").lsp_highlight_document(client)
		require("codethread.lsp.settings.shared").lsp_keymaps(bufnr)
	end,
}
