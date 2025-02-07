vim.opt_local.spell = true
vim.opt_local.spelloptions:append 'camel'
vim.opt_local.spellcapcheck = ''

-- map <leader>ll yiwoconsole.log('\n<C-r>0:', <C-r>0);<C-[>k
-- map <leader>ld :%s/.*console.log.*\n//g<CR>


--[[stylua: ignore]] --format
U.localleader {
	{ 'g' , function() require('codethread.find_node_module').find_node_module() end       , 'Find Modules'        },
	{ 'cc', Cmd '%g/console/norm dd'                                                       , 'Clear logs'          },
	{ 's' , function() require('swap-ternary').swap() end                                  , 'Swap ternary'        },
	{ 'a' , function() require('typescript').actions.addMissingImports { sync = true, } end, 'add missing imports' },
	{ 'o' , function() require('typescript').actions.organizeImports { sync = true } end   , 'organise imports'    },
	{ ',' , function() require('typescript').actions.removeUnused { sync = true } end      , 'Remove unused'       },
}
