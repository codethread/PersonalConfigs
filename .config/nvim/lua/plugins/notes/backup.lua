local Job = require 'plenary.job'
local cwd = require('plugins.notes.constants').cwd

local M = {}

local timer_h

local function schedule(callback)
	if timer_h then
		timer_h:stop()
		timer_h:close()
	end
	timer_h = vim.loop.new_timer()
	timer_h:start(
		1000 * 60 * 3, -- 3 mins
		0,
		function()
			timer_h:stop()
			timer_h:close()
			vim.schedule(callback)
			timer_h = nil
		end
	)
end

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
local function warn(msg) vim.notify(msg, vim.log.levels.WARN, { title = 'Notes', timeout = 0 }) end

local function cmd_pull()
	return Job:new {
		command = 'git',
		args = { 'pull' },
		cwd = cwd,
		on_exit = function(j, code)
			if code == 0 then
				info 'Up to date!'
			else
				vim.print(j:stderr_result())
				warn 'Pull failed!'
			end
		end,
	}
end

local function cmd_push()
	return Job:new {
		command = 'git',
		args = { 'push' },
		cwd = cwd,
		on_exit = function(j, code)
			if code ~= 0 then
				vim.print(j:stderr_result())
				warn 'Push failed'
			end
		end,
	}
end

---@param msg? string
local function cmd_commit(msg)
	-- TODO: could do something in here to add the changed files to the commit?
	return Job:new {
		command = 'git',
		-- NOTE: this relies on global git alias
		-- git config --global alias.add-commit '!git add -A && git commit'
		args = { 'add-commit', '-m', msg or os.date() },
		cwd = cwd,
		on_exit = function(j, code)
			if code ~= 0 then
				vim.print(vim.inspect(j:stderr_result()))
				warn 'Add failed'
			end
		end,
	}
end

--- @param opts ct.StatusOpts Options
function M.status(opts)
	vim.system({ 'git', 'status', '--short' }, { text = true, cwd = cwd }, function(out)
		if out.code ~= 0 then
			vim.print(out.stderr)
			warn 'Could not get git status'
		elseif not out.stdout or out.stdout == '' then
			if opts.on_clean then opts.on_clean() end
		else
			if opts.on_dirty then opts.on_dirty() end
		end
	end)
end

---@param msg? string
---@param current_file? string # if passed will check whether the current file is part of a workspace, used for other plugins calling this function that may not know
function M.update_and_push(msg, current_file)
	if current_file and (not vim.startswith(current_file, cwd)) then return end
	schedule(function()
		M.status {
			on_dirty = function()
				local commit = cmd_commit(msg)
				local push = cmd_push()

				commit:and_then_on_success(push)

				commit:start()
			end,
		}
	end)
end

function M.init()
	info 'Updating'

	local group = vim.api.nvim_create_augroup('ct_obsidian_save', { clear = true })
	vim.api.nvim_create_autocmd({ 'BufWritePost' }, {
		group = group,
		pattern = cwd .. '/*',
		callback = function(opts)
			local new_file = opts.file:gsub(cwd .. '/', '')
			M.update_and_push(new_file)
		end,
	})

	vim.api.nvim_create_autocmd({ 'VimLeavePre' }, {
		group = group,
		callback = function()
			M.status {
				on_dirty = function()
					local commit = cmd_commit()
					local push = cmd_push()

					commit:and_then_on_success(push)

					commit:start()
				end,
			}
		end,
	})

	M.status {
		on_clean = function() cmd_pull():start() end,
		on_dirty = function()
			local pull = cmd_pull()
			local commit = cmd_commit()
			local push = cmd_push()

			commit:and_then_on_success(pull)
			pull:and_then_on_success(push)

			commit:start()
		end,
	}
end

return M

---@class ct.StatusOpts
---@field on_clean? fun(): nil # callback to run if the working tree is clean
---@field on_dirty? fun(): nil # callback to run if the working tree is dirty
