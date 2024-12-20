---Not present in wezterm
local debug = {
	traceback = function(...) print(...) end,
	getinfo = function() end,
}

return debug
