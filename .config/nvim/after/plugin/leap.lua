local require = require('codethread.utils').require
local leap, ok = require 'leap'
if not ok then return end

leap.add_default_mappings()
