-- TIPS
-- to see raw key
-- go to insert mode, type <C-v> then type, and that key will be shown

-- use space as the leader key
vim.keymap.set('', '<Space>', '<Nop>')
vim.g.mapleader = ' '

-- use comma as the localleader key
vim.keymap.set('', ',', '<Nop>')
vim.g.maplocalleader = ','

-- Escape
U.keymap('i', 'jk', '<ESC>')

U.keymap('n', 'ZQ', '<cmd>qa!<cr>') -- default is q!

U.keymap('n', '<M-s>', '<cmd>w<cr>') -- alt or cmd on macos (terminal dependent, works with kitty)

U.keymap('n', '<C-q>', function()
	for _, win in ipairs(vim.api.nvim_tabpage_list_wins(vim.api.nvim_get_current_tabpage())) do
		if vim.fn.getwinvar(win, '&syntax') == 'qf' then
			vim.cmd.cclose()
			return
		end
	end
	vim.cmd.copen()
end, 'toggle quickfix')

local function is_github_link(w)
	local bits = vim.split(w, '/')
	return #bits == 2
end

-- replace missing gx with netrw gone
U.keymap('n', 'gx', function()
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
end, 'Go to link')
