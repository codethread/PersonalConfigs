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
