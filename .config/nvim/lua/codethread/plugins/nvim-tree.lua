local nmap = require('codethread.utils').nmap
local nvim_tree_status_ok, nt = pcall(require, 'nvim-tree')
if not nvim_tree_status_ok then
	print 'could not load nvim_tree'
	return
end

-- turn of netrw and use nvim-tree
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

-- this is <C--> but maps this way :shrug:
nmap('<C-_>', '<cmd>NvimTreeFindFileToggle<cr>')

-- nt.setup()
nt.setup {
	-- hijack_netrw = false,
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
}
