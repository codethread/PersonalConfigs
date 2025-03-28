local keymapper = require 'codethread.keymaps.keymapper'
vim.opt_local.spell = true
vim.opt_local.spelloptions:append 'camel'
vim.opt_local.spellcapcheck = ''

-- map <leader>ll yiwoconsole.log('\n<C-r>0:', <C-r>0);<C-[>k
-- map <leader>ld :%s/.*console.log.*\n//g<CR>

--[[stylua: ignore]] --format
keymapper.localleader {
	{ 'g' , 'Find Modules'       , function() require('codethread.find_node_module').find_node_module() end                     },
	{ 'cc', 'Clear logs'         , Cmd '%g/console/norm dd'                                                                     },
	{ 's' , 'Swap ternary'       , function() require('swap-ternary').swap() end                                                },
	{ 'a' , 'add missing imports', function() require('vtsls').commands.add_missing_imports(vim.api.nvim_get_current_buf()) end },
}
