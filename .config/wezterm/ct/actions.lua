local wezterm = require 'wezterm' --[[@as Wezterm]]
local utils = require 'ct.utils'
local _ = require '_'

local act = wezterm.action

local M = {}

M.runWorkProject = wezterm.action_callback(function(w, p)
	local TARGET = 'work-web'
	local sessions = wezterm.mux.get_workspace_names()

	local running_session = _.list.find(sessions, function(s) return _.str(s):starts_with(TARGET) end)

	if running_session then
		return w:perform_action(
			act.SwitchToWorkspace {
				name = running_session,
			},
			p
		)
	end

	local projects = { 'deals-light-ui', 'fe-review', 'fe-native' }

	local choices = _.list.map(projects, function(label) return { label = label } end)

	w:perform_action(
		act.InputSelector {
			fuzzy = true,
			fuzzy_description = wezterm.format {
				{ Attribute = { Intensity = 'Bold' } },
				{ Foreground = { AnsiColor = 'Fuchsia' } },
				{ Text = 'foo bar' },
			},
			choices = choices,
			action = wezterm.action_callback(
				function(w1, p1, _, line)
					w1:perform_action(
						act.SwitchToWorkspace {
							name = TARGET .. '-' .. line,
							spawn = {
								cwd = utils.home('work/' .. line),
								args = { 'testy' },
							},
						},
						p1
					)
				end
			),
		},
		p
	)
end)

---Fuzzy select urls from all panes to open in current nvim
---assumes pane 1 to be nvim
M.open_file_in_nvim = wezterm.action_callback(function(win, pane)
	local panes = win:active_tab():panes()
	local non_ide_panes = _.list.filter(panes, function(p)
		local ps = _.str(p:get_foreground_process_name())
		return not ps:includes 'nvim'
	end)

	local pane_text = _.list.map(non_ide_panes, function(p)
		local values = {}
		-- this may not be the best
		for value in p:get_logical_lines_as_text():gmatch '%s[%w%.-_/]*[%./][%w%.-_/:]*%s' do
			table.insert(values, value)
		end
		return values
	end)

	local choices = _.list.map(
		_.list.uniq(_.list.flatten(pane_text)),
		function(path) return { label = _.str(path):trim():to_string() } end
	)

	win:perform_action(
		act.InputSelector {
			fuzzy = true,
			fuzzy_description = wezterm.format {
				{ Attribute = { Intensity = 'Bold' } },
				{ Foreground = { AnsiColor = 'Fuchsia' } },
				{ Text = 'Open Link' },
			},
			choices = choices,
			action = wezterm.action_callback(function(_w, _p, _x, url)
				if not url then return end
				wezterm.log_info('opening: ' .. url)
				local path, line, col = table.unpack(_.str(url):split ':')
				M.open_in_nvim(win, path, tonumber(line), tonumber(col))
				win:perform_action(act.ActivatePaneByIndex(0), pane)
			end),
		},
		pane
	)
end)

---Call QuickSelect to grab urls from the opposite pane (only really works with two panes)
---assumes pane 1 to be nvim
M.quick_select_file_for_editor = wezterm.action_callback(function(win, pane)
	-- NOTE: this assumes window panes are iterated in the order of their index, as used by `ActivatePaneByIndex`.
	-- if not, use `wezterm cli activate-pane`
	local is_nvim = _.str(pane:get_foreground_process_name()):includes 'nvim'

	if is_nvim then
		local target = nil
		for i, p in ipairs(win:active_tab():panes()) do
			if not _.str(p:get_foreground_process_name()):includes 'nvim' then
				target = { index = i - 1, pane = p }
				break
			end
		end

		if not target then return end

		win:perform_action(act.ActivatePaneByIndex(target.index), pane)
		wezterm.sleep_ms(10) -- no await on actions
		win:perform_action(
			act.QuickSelectArgs {
				patterns = {
					'[a-zA-Z\\.-_/]*[\\./][a-zA-Z\\.-_/:]*',
				},
				action = wezterm.action_callback(function(window, p)
					local url = window:get_selection_text_for_pane(p)
					wezterm.log_info('opening: ' .. url)
					local path, line, col = table.unpack(_.str(url):split ':')
					M.open_in_nvim(window, path, tonumber(line), tonumber(col))
					window:perform_action(act.ActivatePaneByIndex(0), p)
				end),
			},
			pane
		)
	end
end)

---Open a file in running nvim window
---@param window Window
---@param file string
---@param line number?
---@param col number?
function M.open_in_nvim(window, file, line, col)
	local nvim_pane = _.list.find(
		window:active_tab():panes(),
		function(p) return _.str(p:get_foreground_process_name()):includes 'nvim' end
	)

	if not nvim_pane then
		window:toast_notification('No editor', 'no open editor to handle file', nil, 4000)
		return
	end

	if file and line and col then
		nvim_pane:send_text(':e +call\\ cursor(' .. line .. ',' .. col .. ') ' .. file .. '\r')
	elseif file and line then
		nvim_pane:send_text(':e +' .. line .. ' ' .. file .. '\r')
	elseif file then
		nvim_pane:send_text(':e ' .. file .. '\r')
	end
end

return M
