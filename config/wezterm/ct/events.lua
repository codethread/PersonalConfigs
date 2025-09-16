local wezterm = require 'wezterm' --[[@as Wezterm]]

local M = {}

wezterm.on('user-var-changed', function(window, pane, name, value)
	wezterm.log_info('var', name, value)

	if name == 'event' then
		local event = wezterm.json_parse(value)
		local handler = M.event_handlers[event._tag]
		if not handler then
			window:toast_notification('Nil Event', 'no event handler for: ' .. event._tag, nil, 4000)
		else
			handler(window, pane, event)
		end
	end
end)

M.event_handlers = {
	---@param window Window
	---@param pane Pane
	---@param event { msg: string, info?: string }
	Notify = function(window, pane, event)
		window:toast_notification(event.msg, event.info, nil, 4000)
	end,
}
