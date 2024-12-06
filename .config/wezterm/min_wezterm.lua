print '--|  START   |--'

---Not present in wezterm
debug = {
	traceback = function(...) print(...) end,
	getinfo = function() end,
}
local wezterm = require 'wezterm' --[[@as Wezterm]]

-- add build to package.path because it won't live inside .config dir
package.path = package.path
	.. ';'
	.. wezterm.home_dir
	.. '/PersonalConfigs/.config/wezterm/build/?.lua'

local config = require('build').config --[[@as Config]]

config.max_fps = 120

print '--| RELOADED |--'
return config
