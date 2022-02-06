local status_ok, _ = pcall(require, "lspconfig")
if not status_ok then
  print "lsp failed to load"
  return
end

require("lsp.lsp-installer")
require("lsp.handlers").setup()
require "lsp.null-ls"
