local safe_load = require('codethread.utils').safe_load

---@class CTColorThemeConfig
---@field initial "light" | "dark"
---@field light nil | fun(): nil
---@field dark nil | fun(): nil
---@field statusline nil | string
---@field colors fun(): table | nil
local default = {}

---@class CTTheme
---@field setup fun(opts: CTColorThemeConfig): nil
---@field default CTColorThemeConfig
local M = {
	setup = function(opts)
		-- start up with initial theme, doesn't really matter, but if dark notify doesn't work, we won't get a theme
		opts[opts.initial]()

		-- react to OS theme changes
		safe_load('dark_notify', function(dark_notify)
			dark_notify.run {
				onchange = function(mode) -- light or dark
					if mode == 'light' then
						opts.light()
						vim.cmd [[doautocmd User ThemeChangedLight]]
					else
						opts.dark()
						vim.cmd [[doautocmd User ThemeChangedDark]]
					end

					vim.notify('theme changed => ' .. mode)
				end,
			}
		end)
	end,
	default = default,
}

return M
