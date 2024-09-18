-- require 'codethread.alt'
require 'codethread.dotty'
require 'codethread.movement'
require 'codethread.xstate'
require 'codethread.nushell'
local pomo = require 'codethread.pomo'

local is_work = U.machine {
	work = true,
	home = false,
}

if is_work then pomo.setup() end
