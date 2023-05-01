local utils = require 'codethread.utils'
local nmap = utils.nmap

local require = require('codethread.utils').require
local harpoon, ok = require 'harpoon'
local telescope, ok_2 = require 'codethread.telescope'
if not ok or not ok_2 then return end

harpoon.setup {
	menu = {
		width = vim.api.nvim_win_get_width(0) - 4,
	},
}

function _G.harpoon_keybinds()
	-- nmap('J', function() require('harpoon.mark').n end)
end

-- remapping all the marks commands because marks suck
-- but keeping it under 'M' just in case
nmap('M', 'm')
nmap('ma', function() require('harpoon.mark').add_file() end)
nmap('mf', '<cmd>Telescope harpoon marks<cr>')
nmap('ml', function() require('harpoon.ui').toggle_quick_menu() end)
nmap('mn', function() require('harpoon.ui').nav_next() end)
nmap('mp', function() require('harpoon.ui').nav_prev() end)
nmap('mt', function()
	require('harpoon.term').gotoTerminal(1) -- navigates to term 1
end)

nmap('m1', function() require('harpoon.ui').nav_file(1) end)
nmap('m2', function() require('harpoon.ui').nav_file(2) end)
nmap('m3', function() require('harpoon.ui').nav_file(3) end)
nmap('m4', function() require('harpoon.ui').nav_file(4) end)

vim.cmd [[
            augroup MyHarpoon
                au!
                au FileType harpoon lua harpoon_keybinds()
            augroup end
        ]]

telescope.load_extension 'harpoon'
