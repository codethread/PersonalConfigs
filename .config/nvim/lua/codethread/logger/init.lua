local M = {}

---@module 'snacks'

local log = require 'codethread.logger.plenarylog'

---@type ct.LogLevels[]
local levels = {
	'error',
	'info',
	'debug',
}

---List of loggers to use throughout config, can be controlled globally
---@type { [string]: ct.Loggers }
M.loggers = {}

---Logger for debugging my own config. Builds on plenary log but is controlled
---through `Log` ex command to change at runtime or startup
---Also supports env_var `DEBUG_CT_ ..config.plugin`, e.g `DEBUG_CT_DOTTY`
---@param config ct.LogConfig
---@return ct.Logger
function M.new(config)
	do -- validation of inputs
		if not config or not config.plugin then error 'log.new needs config and plugin' end

		vim.validate('config.level', config.level, function(l)
			local ok = vim.list_contains(levels, l)
			if ok then return ok end
			return ok, string.format('expected %s to be one of %s', l, table.concat(levels, ', '))
		end, true)
	end

	config.use_console = config.use_console or false
	config.level = config.level or os.getenv('DEBUG_CT_' .. config.plugin:upper()) or 'error'

	-- create a log file automatically and pass to plenary
	---@diagnostic disable-next-line: inject-field
	config.outfile = not config.use_console
			and (vim.fn.stdpath 'cache' .. '/' .. config.plugin .. '.log')
		or nil

	local logger = log.new(config)

	M.loggers[config.plugin] = {
		logger = logger,
		config = config,
		file = config.outfile,
	}

	return logger
end

---Set the log level for a given logger GLOBALLY
---@param logger string
---@param level ct.LogLevels
function M.set(logger, level)
	vim.validate('logger', logger, 'string')
	vim.validate('level', level, function(l)
		local ok = vim.list_contains(levels, l)
		if ok then return ok end
		return ok, string.format('expected %s to be one of %s', l, table.concat(levels, ', '))
	end)

	local instance = M.loggers[logger]
	if not instance then
		vim.notify(
			string.format(
				'No logger with name %s, valid options: %s',
				logger,
				table.concat(vim.tbl_keys(M.loggers), ' | ')
			)
		)
	end

	local opts = vim.tbl_extend('force', instance.config, { level = level })
	M.new(opts)
end

---@alias ct.LogLevels 'debug' | 'info' | 'error'

---@class ct.Loggers
---@field logger ct.Logger instance of a logger
---@field config ct.LogConfig config for reconstructing logger on level change
---@field file? string log file

---@class ct.LogConfig
---@field plugin string Name of the plugin. Prepended to log messages.
---@field use_console? boolean Will log to messages, else logs to `stdpath("cache")/plugin` [default: false]
---@field level? 'debug' | 'info' | 'error' level for logger [default: error]

---@param arg_lead string
---@param cmd_line string
---@param cursor_pos number
---@return string[]
local function completion_set(arg_lead, cmd_line, cursor_pos)
	local count = 0
	-- split on spaces to get the user typing in something Log <logger> <level>
	for _ in cmd_line:gmatch ' ' do
		count = count + 1
	end

	if count > 1 then -- show level completions
		return levels
	else -- show logger completions
		return vim.tbl_keys(M.loggers)
	end
end

vim.api.nvim_create_user_command('LogSet', function(opts)
	local logger = opts.fargs[1]
	local level = opts.fargs[2]
	M.set(logger, level)
end, {
	desc = 'Set the log level for a given logger GLOBALLY',
	nargs = '+',
	complete = completion_set,
})

vim.api.nvim_create_user_command('LogOpen', function(opts)
	local logger = opts.fargs[1]
	local file = M.loggers[logger].file

	Snacks.win.new {
		position = 'bottom',
		minimal = true,
		enter = true,
		file = file,
		fixbuf = true,
		wo = { wrap = false },
		keys = {
			q = 'close',
			t = function() vim.wo.wrap = not vim.wo.wrap end,
		},
	}
end, {
	desc = 'Open log file',
	nargs = 1,
	complete = function() return vim.tbl_keys(M.loggers) end,
})

return M
