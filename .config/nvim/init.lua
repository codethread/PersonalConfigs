local og_req = require
local require = function(lib)
	local ok = pcall(og_req, lib)
	if not ok then vim.notify('could not load ' .. lib) end
end

require 'codethread.settings'
require 'codethread.keymaps'
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
require 'codethread.dashboard'

require 'xstate'
require 'dotty'
require 'qmk'

-- Playing around with oxi vim
-- vim.cmd([[
-- set rtp+=~/dev/nvim-rs
-- ]])

-- let g:indentLine_fileTypeExclude = ['help', 'man', 'startify', 'NERDTree', 'netrw', 'gf']
--
-- map <leader>ll yiwoconsole.log('\n<C-r>0:', <C-r>0);<C-[>k
-- map <leader>ld :%s/.*console.log.*\n//g<CR>
