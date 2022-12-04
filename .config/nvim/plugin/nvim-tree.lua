local nmap = require('codethread.utils').nmap
local nvim_tree_status_ok, nt = pcall(require, 'nvim-tree')
if not nvim_tree_status_ok then
	print 'could not load nvim_tree'
	return
end

-- turn off netrw and use nvim-tree
vim.g.loaded_netrw = 0
vim.g.loaded_netrwPlugin = 0

nmap('<C-n>', '<cmd>NvimTreeFindFileToggle<cr>')

nt.setup {
	hijack_netrw = true,
	disable_netrw = true,
	filters = { -- remove things from view
		dotfiles = false,
	},
	view = {
		adaptive_size = true,
	},
	actions = {
		change_dir = {
			enable = false, -- stay in the current directory
		},
		open_file = {
			-- quit_on_open = true,
			window_picker = {
				chars = 'jfkdlsa;',
			},
		},
	},
	-- remove_keymaps = {
	-- 	'<C-e>',
	-- },
	-- on_attatch = function (bufnr)
	--     nmap('<C-e>', '<cmd>NvimTreeFindFileToggle<cr>', { buffer = bufnr })
	-- end
}

local function is_github_link(w)
	local bits = vim.split(w, '/')
	return #bits == 2
end

-- replace missing gx with netrw gone
vim.keymap.set('n', 'gx', function()
	local file = vim.fn.expand '<cfile>'
	local Job = require 'plenary.job'

	if vim.startswith(file, 'http') then
		Job:new({
			command = 'open',
			args = { file },
		}):sync()
	elseif is_github_link(file) then
		Job:new({
			command = 'open',
			args = { 'https://github.com/' .. file },
		}):sync()
	else
		print('not a link: ' .. file)
	end
end, {})
