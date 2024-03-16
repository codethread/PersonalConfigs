local Job = require 'plenary.job'

local M = {}

---notify at info level
---@param msg string
local function info(msg)
	vim.notify(msg, vim.log.levels.INFO, {
		title = 'Notes',
		hide_from_history = true,
		timeout = 0,
	})
end

---notify at warn level
---@param msg string
local function warn(msg)
	vim.notify(msg, vim.log.levels.WARN, {
		title = 'Notes',
		hide_from_history = true,
		timeout = 0,
	})
end

local add = Job:new {
	command = 'git',
	args = { 'add', '.' },
	on_exit = function(_, code)
		if code ~= 0 then warn 'Add failed' end
	end,
}

-- TODO: could pass files changed to this from status for more info
local commit = Job:new {
	command = 'git',
	args = { 'commit', '-m', os.date() },
	on_exit = function(_, code)
		if code ~= 0 then warn 'Add failed' end
	end,
}

local push = Job:new {
	command = 'git',
	args = { 'push' },
	on_exit = function(_, code)
		if code == 0 then
			info 'Updated Remote'
		else
			warn 'Push failed'
		end
	end,
}

function M.update()
	M.status {
		on_dirty = function()
			add:and_then_on_success(commit)
			commit:and_then_on_success(push)
			add:start()
		end,
	}
end

--- @param opts ct.StatusOpts Options
function M.status(opts)
	vim.system({ 'git', 'status', '--short' }, { text = true }, function(out)
		if out.code ~= 0 then
			warn 'Could not get git status'
		elseif not out.stdout or out.stdout == '' then
			if opts.on_clean then opts.on_clean() end
		else
			if opts.on_dirty then opts.on_dirty() end
		end
	end)
end

local timer = vim.loop.new_timer()

local pull_and_start = Job:new {
	command = 'git',
	args = { 'pull' },
	on_exit = function(_, code)
		if code == 0 then
			info 'Up to date!'
			local interval = 1000 * 60 * 5 -- 5mins
			timer:start(0, interval, function() M.update() end)
		else
			warn 'Pull failed!'
		end
	end,
}
function M.init()
	info 'Updating'

	M.status {
		on_clean = function() pull_and_start:start() end,
		on_dirty = function()
			M.update()

			-- HACK: to wait on the above
			vim.defer_fn(function() pull_and_start:start() end, 5000)
		end,
	}
end

return M

---@class ct.StatusOpts
---@field on_clean? fun(): nil # callback to run if the working tree is clean
---@field on_dirty? fun(): nil # callback to run if the working tree is dirty
