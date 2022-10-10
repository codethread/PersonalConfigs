local M = {}

vim.opt.termguicolors = true -- adds more colors

local config = {

	style = 'storm',
	transparent = true,
	day_brightness = 0.25,
	hide_inactive_statusline = true,
	dim_inactive = true,
	on_highlights = function(hl, c)
		hl.TSKeywordReturn = {
			fg = c.orange,
		}
		hl.LineNr = {
			fg = c.green2,
		}
		-- hl.CursorLineNr = {
		-- 	fg = c.green1,
		-- }
	end,
}

function M.setup()
	local tokyonight_status_ok, tk = pcall(require, 'tokyonight')
	if not tokyonight_status_ok then
		print 'could not load tokyonight'
		return
	end

	tk.setup(config)

	vim.cmd [[colorscheme tokyonight]]

	local status_ok, dark_notify = pcall(require, 'dark_notify')
	if not status_ok then
		print 'could notload dark-notify'
	else
		dark_notify.run {
			onchange = function(mode) -- light or dark
				vim.g.tokyonight_style = mode == 'light' and 'day' or 'storm'
				vim.cmd [[colorscheme tokyonight]]
				if mode == 'light' then
					vim.cmd [[doautocmd User ThemeChangedLight]]
				else
					vim.cmd [[doautocmd User ThemeChangedDark]]
				end
			end,
		}
	end
end

M.lualine = 'tokyonight'

local group = vim.api.nvim_create_augroup('ThemeChanged', { clear = true })

-- register callbacks for when color theme changes
function M.on_change(fn)
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
