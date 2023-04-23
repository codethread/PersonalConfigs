local events = require 'codethread.events'

---@return "light" | "dark"
local function get_initial()
	local conf_file = os.getenv 'DOTFILES' .. '/.config/kitty/kitty.conf'
	local cmd = 'cat ' .. conf_file .. ' | grep "# Tokyo Night"'

	local res = vim.trim(vim.fn.system(cmd))
	local mode = vim.endswith(res, 'Day') and 'light' or 'dark'
	return mode
end

---@class CTColorThemeConfig
---@field initial "light" | "dark"
---@field light nil | fun(): table
---@field dark nil | fun(): table
---@field statusline nil | string
---@field colors fun(): table | nil
local default = {}

---@class CTTheme
---@field setup fun(opts: CTColorThemeConfig): nil
---@field default CTColorThemeConfig
local M = {
	setup = function(opts)
		-- start with a nice blank slate to prevent flashing
		vim.cmd [[
            colorscheme quiet
            highlight Normal guibg=none
            highlight NonText guibg=none
        ]]

		local require = require('codethread.utils').require
		local dark_notify, ok = require 'dark_notify'
		if not ok then
			-- start up with initial theme, doesn't really matter, but if dark notify doesn't work, we won't get a theme
			local mode = get_initial()
			local colors = opts[mode]()
			vim.defer_fn(function() events.emit('ColorScheme', mode, colors) end, 500)
			return
		end

		-- react to OS theme changes
		dark_notify.run {
			onchange = function(mode) -- light or dark
				if mode == 'light' then
					local colors = opts.light()
					events.emit('ColorScheme', mode, colors)

					-- vim.cmd [[doautocmd User ThemeChangedLight]]
				else
					local colors = opts.dark()
					events.emit('ColorScheme', mode, colors)
					-- vim.cmd [[doautocmd User ThemeChangedDark]]
				end

				-- vim.notify('theme changed => ' .. mode)
			end,
		}
	end,
	default = default,
}

return M
