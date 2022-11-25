vim.opt.termguicolors = true -- adds more colors

local theme = require 'codethread.themes.nord'
-- local theme = require 'codethread.themes.onenord'
-- local theme = require 'codethread.themes.nordic'

local group = vim.api.nvim_create_augroup('ThemeChanged', { clear = true })

theme.setup()

return {
	lualine = theme.lualine,

	-- register callbacks for when color theme changes
	on_change = function(fn)
		local light = theme.on_change_light
		local dark = theme.on_change_dark

		if light then
			vim.api.nvim_create_autocmd('User', {
				callback = function() dark(fn) end,
				group = group,
				pattern = 'ThemeChangedDark',
			})
		end

		if dark then
			vim.api.nvim_create_autocmd('User', {
				callback = function() light(fn) end,
				group = group,
				pattern = 'ThemeChangedLight',
			})
		end
	end,
}
