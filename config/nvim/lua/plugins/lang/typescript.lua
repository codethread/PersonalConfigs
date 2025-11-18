-- TypeScript LSP is now handled by vtsls in lspconfig.lua
-- This file is kept for twoslash-queries plugin integration
local server = 'vtsls'
return {
	{
		'marilari88/twoslash-queries.nvim',
		ft = U.ecma_ft,
		init = function()
			U.lsp_attach(
				server,
				function(client, buffer) require('twoslash-queries').attach(client, buffer) end
			)
		end,
		opts = {},
	},
}
