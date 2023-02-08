local require = require('codethread.utils').require
require 'lspconfig'
require('mason').setup()
require 'codethread.lsp.setup'
require('codethread.lsp.diagnostics').setup()
require 'codethread.lsp.null-ls'
require 'codethread.lsp.dap'
