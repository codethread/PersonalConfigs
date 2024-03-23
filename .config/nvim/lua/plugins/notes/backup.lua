local Job = require 'plenary.job'

local M = {}

M.cwd = vim.fn.expand '~' .. '/Library/Mobile Documents/iCloud~md~obsidian/Documents/Notes'

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
		cwd = M.cwd,
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
		cwd = M.cwd,
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
		cwd = M.cwd,
		on_exit = function(j, code)
			if code ~= 0 then
				vim.print(j:stderr_result())
				warn 'Add failed'
			end
		end,
	}
end

--- @param opts ct.StatusOpts Options
function M.status(opts)
	vim.system({ 'git', 'status', '--short' }, { text = true, cwd = M.cwd }, function(out)
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
	if current_file and (not vim.startswith(current_file, M.cwd)) then return end
	local commit = cmd_commit(msg)
	local push = cmd_push()

	commit:and_then_on_success(push)

	commit:start()
end

function M.init()
	info 'Updating'

	vim.api.nvim_create_autocmd({ 'BufWritePost' }, {
		group = vim.api.nvim_create_augroup('ct_obsidian_save', { clear = true }),
		pattern = M.cwd .. '/*',
		callback = function(opts)
			local new_file = opts.file:gsub(M.cwd .. '/', '')
			M.update_and_push(new_file)
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
