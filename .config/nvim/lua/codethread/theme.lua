local M = {}

function M.setup()
	vim.g.background = "light"
	vim.g.nord_italic = false -- this makes too many things italic
	vim.g.nord_borders = true
	vim.g.nord_contrast = true

	local status_ok, nord = pcall(require, "nord")
	if not status_ok then
		print("could not load nord")
		return
	end

	nord.set()

	local nord_colors = require("nord.colors")
	local colors = {
		-- darks
		bg = nord_colors.nord0_gui, -- used for background and area coloring
		bg_bright = nord_colors.nord1_gui, -- elevated, more prominent or focused UI elements like status bars
		bg_brightest = nord_colors.nord2_gui, -- used to colorize the currently active text editor line as well as selection
		subtle = nord_colors.nord3_gui, -- used for comments and UI elements like indent

		-- lights
		white = nord_colors.nord4_gui, -- text and caret
		bright = nord_colors.nord5_gui, -- not much visual attention or subtle, e.g. current line
		brightest = nord_colors.nord6_gui, -- prominent text like brackets

		-- blues
		emph = nord_colors.nord7_gui, -- classes, types etc
		primary = nord_colors.nord8_gui, -- most visual attention, functions, methods
		seconary = nord_colors.nord9_gui, -- some attention, e.g keywords, commas
		tertiary = nord_colors.nord10_gui, --

		-- colors
		red = nord_colors.nord11_gui,
		orange = nord_colors.nord12_gui,
		yellow = nord_colors.nord13_gui,
		green = nord_colors.nord14_gui,
		purple = nord_colors.nord15_gui,
	}
	-- nord3_gui_bright = "#616E88", -- out of palette

	-- global overrides
	vim.cmd("hi TSKeywordReturn gui=bold guifg=" .. colors.orange)
	vim.cmd("hi TSComment gui=italic")
	vim.cmd("hi TSKeyword gui=bold guifg=" .. colors.primary)

	-- typescript
	vim.cmd("hi tsxTSProperty gui=italic guifg=" .. colors.seconary)
	vim.cmd("hi typescriptTSProperty gui=italic guifg=" .. colors.seconary)
end

M.lualine = "nord"

return M
