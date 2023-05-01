local require = require('codethread.utils').require
local neoclip, ok_1 = require 'neoclip'
local telescope, ok_2 = require 'codethread.telescope'
if not ok_1 or not ok_2 then return end

neoclip.setup()
telescope.load_extension 'neoclip'
