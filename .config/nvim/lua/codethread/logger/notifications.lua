--- Notifications handles a 'drawer' for logs and messages.
---
--- notifications from Snacks (including getting history and piping to a buffer,
--- with updates).
---
--- Also works with my log implementation to grab from there if requested.
---
--- TODO:
--- - logs need aligning a little better (formats are different)
--- - does this need a keybinding?
--- - should other windows get closed?
local M = {}

---@module 'snacks'

local obj = require 'lib.obj'
local log = require('codethread.logger.init').new { plugin = 'popup' }

---@returns ct.logs.state
local function default_state() return { win = nil, last_id = 0 } end

---@type table<string, ct.logs.state>
local windows = {
	notifications = default_state(),
}

---Take an iterable list of notifications and convert them to strings for
---insertion into a buffer
---@param state ct.logs.state
---@param iter Iter
---@return string[]
local function format_notifications(state, iter)
	return iter
		:map(
			---@param n snacks.notifier.Notif
			function(n)
				state.last_id = n.id --[[@as integer]]
				return vim.split(
					string.format('%s [%s]: %s', os.date('%H:%M:%S', n.added), n.level, n.msg),
					'\n'
				)
			end
		)
		:flatten(1)
		:totable()
end

---Create a window for the given state
---@param state ct.logs.state
local function create_win(state)
	log.info 'Creating window'

	local win = Snacks.win.new {
		position = 'bottom',
		minimal = true,
		enter = true,
		buf = state.buf,
		file = state.file,
		fixbuf = true,
		wo = { wrap = false },
		keys = {
			q = 'close',
			t = function() vim.wo.wrap = not vim.wo.wrap end,
		},
	}

	state.win = win
	log.debug('created window', win)
end

---Update the history in the state `buf
---Mutates `state`
---@param state ct.logs.state
local function update_notifications_buf(state)
	log.info 'Update'
	local notifications = Snacks.notifier.get_history {}
	log.debug('skipping', state.last_id)
	local skipped = vim.iter(notifications):skip(state.last_id)
	log.debug('first notification:', skipped:peek())
	local lines = format_notifications(state, skipped)
	vim.api.nvim_buf_set_lines(state.buf, -1, -1, false, lines)
	log.debug('state', obj.omit(state, { 'win' }))
end

---Get snacks notifications and store them in a new buffer
---@param state ct.logs.state
local function get_notifications(state)
	log.info 'Get Notifications'

	local notifications = Snacks.notifier.get_history {}
	log.debug('notification sample:', notifications[1], notifications[2])

	local lines = format_notifications(state, vim.iter(notifications))

	log.debug('formatted sample:', lines[1])
	log.debug('state', obj.omit(state, { 'win' }))
	if #notifications == 0 then
		vim.notify 'no notifications'
		return
	end

	do -- check assumptions about internal parts of snacks
		-- if these fail, update how to iterate through the notifications
		local _1 = notifications[1].id
		local _2 = notifications[2].id
		assert(
			_1 == 1 or not _1,
			string.format("notifcations aren't 1 indexed, got %s, this module needs rethinking", _1)
		)
		assert(
			_2 == 2 or not _2,
			string.format("notifcations aren't incrementing, got %s, this module needs rethinking", _2)
		)
	end

	---@type integer
	local buf
	do -- create a log buffer for our messages
		local ok = pcall(require, 'log-highlight')
		if not ok then
			vim.notify('Expected a log syntax library. Highlights will be missing', vim.log.levels.WARN)
		end
		buf = vim.api.nvim_create_buf(false, true)
		vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
		vim.api.nvim_set_option_value('ft', 'log', { buf = buf })
	end

	state.buf = buf
	log.debug('state', obj.omit(state, { 'win' }))
end

---Toggle a log buffer
---@param type string type of log buffer to read, should match `log.plugin` name or 'messages' or 'notifications'
function M.pop(type)
	log.info('Pop', type)

	if type == 'notifications' then
		local state = windows.notifications
		if not state.win then
			get_notifications(state)
			create_win(state)
		else
			update_notifications_buf(state)
			state.win:toggle()
		end
	else
		local state = windows[type] or default_state()
		log.debug(state)
		local log_info = require('codethread.logger.init').loggers[type]
		log.debug(obj.omit(log_info, { 'logger' }))

		if not log_info or not log_info.file then
			vim.notify(string.format('No logs created for %s', type), vim.log.levels.WARN)
			return
		end

		if not state.win then
			log.info 'Creating log buffer'
			state.file = log_info.file
			create_win(state)
		else
			log.info 'Updating log buffer'
			state.win:toggle()
		end
	end
end

---@class ct.logs.state
---@field win? snacks.win
---@field last_id number
---@field file? string
---@field buf? number

return M
