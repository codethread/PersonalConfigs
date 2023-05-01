local require = require('codethread.utils').require
require 'codethread.settings'
require 'codethread.keymaps'
require 'codethread.globals'
require 'codethread.plugins'
require 'codethread.themes'
require 'codethread.statusline'
require 'codethread.autocommands'
require 'codethread.movement'
require 'codethread.completion'
require 'codethread.telescope'
require 'codethread.git'
require 'codethread.whichkey'
require 'codethread.lsp'
require 'codethread.treesitter'
require 'codethread.alt'

vim.defer_fn(U.get_failed_modules, 100)
