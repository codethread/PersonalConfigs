local wezterm = require 'wezterm' --[[@as Wezterm]]
local _ = require '_'
local actions = require 'ct.actions'

local M = {}

---comment
---@param config Config
function M.apply_to_config(config)
	config.hyperlink_rules = wezterm.default_hyperlink_rules()

	table.insert(config.hyperlink_rules, {
		regex = '[/.A-Za-z0-9_-]+\\.[A-Za-z0-9]+(:\\d+)*(?=\\s*|$)',
		format = '$EDITOR://$0',
	})
end

wezterm.on('open-uri', function(window, _pane, uri)
	local link = _.str(uri)
	wezterm.log_info('clicked: ', uri)

	if link:starts_with 'file:' then
		local file = link:replace('file://', '')
		actions.open_in_nvim(window, file:to_string())
		return false
	end

	if link:starts_with '$EDITOR' then
		local file = link:replace('$EDITOR://', '')
		local path, line, col = table.unpack(file:split ':')
		actions.open_in_nvim(window, path, tonumber(line), tonumber(col))
		return false
	end
end)

return M
