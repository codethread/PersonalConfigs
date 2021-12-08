local nnoremap = require('astronauta.keymap').nnoremap

nnoremap { 'gd', function() vim.lsp.buf.definition() end }
nnoremap { 'gr', function() vim.lsp.buf.references() end }
nnoremap { 'K', function() vim.lsp.buf.hover() end }
