local log = require 'plenary.log'

---@type ct.LogLevels[]
local levels = {
	'error',
	'info',
	'debug',
}

---List of loggers to use throughout config, can be controlled globally
---@type { [string]: ct.Loggers }
local loggers = {}

local M = {}

---Logger for debugging my own config. Builds on plenary log but is controlled
---through global config to change at runtime or startup
---@param config ct.LogConfig
---@return ct.Logger
function M.new(config)
	if not config or not config.plugin then error 'log.new needs config and plugin' end
	vim.validate('config.level', config.level, function(l)
		local ok = vim.list_contains(levels, l)
		if ok then return ok end
		return ok, string.format('expected %s to be one of %s', l, table.concat(levels, ', '))
	end, true)

	config.level = config.level or os.getenv('DEBUG_CT_' .. config.plugin:upper()) or 'error'

	local logger = log.new(config, true)

	loggers[config.plugin] = {
		logger = logger,
		config = config,
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

	local instance = loggers[logger]
	if not instance then
		vim.notify(
			string.format(
				'No logger with name %s, valid options: %s',
				logger,
				table.concat(vim.tbl_keys(loggers), ' | ')
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

---@class ct.LogConfig
---@field plugin string Name of the plugin. Prepended to log messages.
---@field level? 'debug' | 'info' | 'error' level for logger [default: error]

---@class ct.Logger
---@field trace fun(...: any): nil
---@field debug fun(...: any): nil
---@field info fun(...: any): nil
---@field warn fun(...: any): nil
---@field error fun(...: any): nil
---@field fatal fun(...: any): nil
---@field fmt_trace fun(...: any): nil fmt_trace("These are %s strings", "formatted")
---@field fmt_debug fun(...: any): nil fmt_debug("These are %s strings", "formatted")
---@field fmt_info fun(...: any): nil fmt_info("These are %s strings", "formatted")
---@field fmt_warn fun(...: any): nil fmt_warn("These are %s strings", "formatted")
---@field fmt_error fun(...: any): nil fmt_error("These are %s strings", "formatted")
---@field fmt_fatal fun(...: any): nil fmt_fatal("These are %s strings", "formatted")
---@field file_trace fun(...: any): nil
---@field file_debug fun(...: any): nil
---@field file_info fun(...: any): nil
---@field file_warn fun(...: any): nil
---@field file_error fun(...: any): nil
---@field file_fatal fun(...: any): nil
---@field lazy_trace fun(...: any): nil
---@field lazy_debug fun(...: any): nil
---@field lazy_info fun(...: any): nil
---@field lazy_warn fun(...: any): nil
---@field lazy_error fun(...: any): nil
---@field lazy_fatal fun(...: any): nil

---@param arg_lead string
---@param cmd_line string
---@param cursor_pos number
---@return string[]
local function completion(arg_lead, cmd_line, cursor_pos)
	local count = 0
	-- split on spaces to get the user typing in something Log <logger> <level>
	for _ in cmd_line:gmatch ' ' do
		count = count + 1
	end

	if count > 1 then -- show level completions
		return levels
	else -- show logger completions
		return vim.tbl_keys(loggers)
	end
end

vim.api.nvim_create_user_command('Log', function(opts)
	local logger = opts.fargs[1]
	local level = opts.fargs[2]
	M.set(logger, level)
end, {
	desc = 'Set the log level for a given logger GLOBALLY',
	nargs = '+',
	complete = completion,
})

return M
