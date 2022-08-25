local M = {}

vim.opt.termguicolors = true -- adds more colors

local function tokyonightTheme()
	vim.g.tokyonight_day_brightness = 0.25 -- Adjusts the brightness of the colors of the Day style. Number between 0 and 1, from dull to vibrant colors

	local status_ok, dark_notify = pcall(require, 'dark_notify')
	if not status_ok then
		print 'could notload dark-notify'
	else
		dark_notify.run {
			onchange = function(mode) -- light or dark
				vim.g.background = mode
				vim.g.tokyonight_style = mode == 'light' and 'day' or 'storm'
				vim.g.tokyonight_hide_inactive_statusline = true
				vim.g.tokyonight_transparent = true
				vim.g.tokyonight_dark_float = false

				vim.cmd [[colorscheme tokyonight]]

				if mode == 'light' then
					vim.cmd [[doautocmd User ThemeChangedLight]]
				else
					vim.cmd [[doautocmd User ThemeChangedDark]]
				end
			end,
		}
	end

	local colors = require('tokyonight.colors').setup {}

	vim.cmd('hi TSKeywordReturn gui=bold guifg=' .. colors.orange)
end

M.setup = tokyonightTheme

M.lualine = 'tokyonight'

local group = vim.api.nvim_create_augroup('ThemeChanged', { clear = true })

-- register callbacks for when color theme changes
M.on_change = function(fn)
	vim.api.nvim_create_autocmd('User', {
		callback = function()
			vim.notify 'dark'
			local colors = require('tokyonight.colors').setup {}
			fn(colors, 'dark')
		end,
		group = group,
		pattern = 'ThemeChangedDark',
	})

	vim.api.nvim_create_autocmd('User', {
		callback = function()
			vim.notify 'light'
			local colors = require('tokyonight.colors').setup {}
			fn(colors, 'light')
		end,
		group = group,
		pattern = 'ThemeChangedLight',
	})
end

return M
