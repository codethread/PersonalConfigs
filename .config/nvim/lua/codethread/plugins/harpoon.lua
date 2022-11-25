local utils = require 'codethread.utils'
local nmap = utils.nmap

local safe_load = require('codethread.utils').safe_load

function _G.harpoon_keybinds()
	-- nmap('J', function() require('harpoon.mark').n end)
end

safe_load('harpoon', function()
	-- remapping all the marks commands because marks suck
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
end)
