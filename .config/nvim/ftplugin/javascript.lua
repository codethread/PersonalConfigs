vim.opt_local.spell = true
vim.opt_local.spelloptions:append 'camel'
vim.opt_local.spellcapcheck = ''

-- map <leader>ll yiwoconsole.log('\n<C-r>0:', <C-r>0);<C-[>k
-- map <leader>ld :%s/.*console.log.*\n//g<CR>

---@class ct.keymapOpts
---@field [1] string
---@field [2] string | function
---@field [3]? string
---@field desc? string
---@field mode? string|string[]

---Create local mappings
--- TODO: move to utils
---
---@param opts ct.keymapOpts[]
local function localleader(opts)
	-- only set bindings for buffer once (is an issue if runtime files are composesd with `unique` keys)
	-- TODO: migrate to which key and use checkhealth for clashes instead
	vim.b.ct_set_bindings = vim.b.ct_set_bindings or false
	if vim.b.ct_set_bindings then return end

	---@type vim.keymap.set.Opts
	local defaults = { buffer = true, silent = true, unique = true }

	for _, keyset in ipairs(opts) do
		local lhs = keyset[1]
		local rhs = keyset[2]
		local desc = keyset[3]

		local set_opts = vim.deepcopy(defaults, true)
		set_opts.desc = desc

		for opt_key, value in pairs(keyset) do
			if type(opt_key) == 'string' and opt_key ~= 'mode' then set_opts[opt_key] = value end
		end

		Try(
			function() vim.keymap.set(keyset.mode or { 'n' }, '<localleader>' .. lhs, rhs, set_opts) end
		)
		vim.b.ct_set_bindings = true
	end
end

--[[stylua: ignore]] --format
localleader {
	{ 'g' , function() require('codethread.find_node_module').find_node_module() end       , 'Find Modules'        },
	{ 'cc', Cmd '%g/console/norm dd'                                                       , 'Clear logs'          },
	{ 's' , function() require('swap-ternary').swap() end                                  , 'Swap ternary'        },
	{ 'a' , function() require('typescript').actions.addMissingImports { sync = true, } end, 'add missing imports' },
	{ 'o' , function() require('typescript').actions.organizeImports { sync = true } end   , 'organise imports'    },
	{ ',' , function() require('typescript').actions.removeUnused { sync = true } end      , 'Remove unused'       },
}
