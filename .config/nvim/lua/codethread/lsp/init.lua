local status_ok, _ = pcall(require, 'lspconfig')
if not status_ok then
	print 'lsp failed to load'
	return
end

require 'codethread.lsp.lsp-installer'
require('codethread.lsp.handlers').setup()
require('codethread.lsp.diagnostics').setup()
require 'codethread.lsp.null-ls'
