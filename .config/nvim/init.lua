require 'codethread.settings'
require 'codethread.keymaps'
require 'codethread.plugins'
require('codethread.theme').setup()
require 'codethread.statusline'
require 'codethread.autocommands'
require 'codethread.movement'
require 'codethread.completion'
require 'codethread.telescope'
require 'codethread.autopairs'
require 'codethread.git'
require 'codethread.whichkey'
require 'codethread.terminal'
require 'codethread.xstate'
require 'codethread.dotty'
require 'codethread.lsp'
require 'codethread.treesitter'
require 'codethread.plugins.ufo'

-- Playing around with oxi vim
-- vim.cmd([[
-- set rtp+=~/dev/nvim-rs
-- ]])

-- let g:indentLine_fileTypeExclude = ['help', 'man', 'startify', 'NERDTree', 'netrw', 'gf']
--
-- map <leader>ll yiwoconsole.log('\n<C-r>0:', <C-r>0);<C-[>k
-- map <leader>ld :%s/.*console.log.*\n//g<CR>
