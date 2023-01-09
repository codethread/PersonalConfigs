local og_req = require
local require = function(lib)
	local ok, loaded = pcall(og_req, lib)
	if not ok then vim.notify('could not load ' .. lib .. '\n' .. loaded) end
	return loaded
end

require 'lspconfig'

require('mason').setup()
require 'codethread.lsp.setup'
require('codethread.lsp.diagnostics').setup()
require 'codethread.lsp.null-ls'
require 'codethread.lsp.dap'
