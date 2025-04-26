local keymapper = require 'codethread.keymaps.keymapper'
vim.opt_local.spell = true
vim.opt_local.spelloptions:append 'camel'
vim.opt_local.spellcapcheck = ''

-- map <leader>ll yiwoconsole.log('\n<C-r>0:', <C-r>0);<C-[>k
-- map <leader>ld :%s/.*console.log.*\n//g<CR>

local qf_id = nil
local function update_quickfix(entries)
	if not entries or vim.tbl_isempty(entries) then return end
	if qf_id then
		require('codethread.fns').append_to_quickfix(entries)
	else
		vim.fn.setqflist({}, ' ', { title = 'File references', items = entries })
		vim.api.nvim_command 'botright copen'
		qf_id = true
	end
end

require('vtsls').config {
	handlers = {
		file_references = function(err, locations)
			if err then return nil end
			local client = unpack(vim.lsp.get_clients { name = 'vtsls' })
			local entries = vim.lsp.util.locations_to_items(locations, client.offset_encoding)
			update_quickfix(entries)
		end,
	},
}

---Search for all imports to the current file
local function find_refs()
	-- TODO: normalise to package.json import
	qf_id = nil

	local quote_char = '"' -- TODO: dynamic based on project, perhaps read prettierrc
	local workspace_chars = [[(@|\.|\.\.)]] -- TODO: dynamic for projects
	local ending = [[$]] -- TODO: handle semicolon based on project
	local file = vim.fn.expand '%:r'
	local filename, parent = unpack(vim.iter(vim.split(file, '/')):rev():totable())
	local resolved_file = vim.startswith(filename, 'index') and '' or '/' .. filename
	local search = table.concat {
		'^(}|import .*) from ', -- handle single or multiline imports
		quote_char,
		workspace_chars,
		[[[/\w-]*]], -- valid import string chars like foo/bar/bax-bing
		parent, --  TODO: extract out
		resolved_file,
		quote_char,
		ending,
	}

	vim.cmd [[VtsExec file_references]]

	local cmd = { 'rg', '--hidden', '--vimgrep', search }
	vim.system(cmd, { text = true }, function(out)
		vim.schedule(function()
			assert(
				out.code == 0,
				string.format(
					'Search failed with code %s.\n---\n%s\n---\nout: %s\n\nerr: %s\n',
					out.code,
					table.concat(cmd, ' '),
					out.stdout,
					out.stderr
				)
			)
			local entries = require('codethread.fns').ripgrep_to_quickfix(out.stdout)
			update_quickfix(entries)
		end)
	end)
end

--[[stylua: ignore]] --format
keymapper.localleader {
	{ 'g' , 'Find Modules'       , function() require('codethread.find_node_module').find_node_module() end                     },
	{ 'cc', 'Clear logs'         , Cmd '%g/console/norm dd'                                                                     },
	{ 's' , 'Swap ternary'       , function() require('swap-ternary').swap() end                                                },
	{ 'a' , 'add missing imports', function() require('vtsls').commands.add_missing_imports(vim.api.nvim_get_current_buf()) end },
	{ 'i' , 'find imports'       , find_refs                                                                                    },
}
