-- require 'codethread.alt'
require 'codethread.dotty'
require 'codethread.movement'
require 'codethread.xstate'
require 'codethread.nushell'
require 'codethread.keymaps.commands'

local pomo = require 'codethread.pomo'

local is_work = U.machine {
	work = true,
}

if is_work == true then pomo.setup() end
