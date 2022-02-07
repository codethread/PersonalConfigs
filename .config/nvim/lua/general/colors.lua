vim.g.nord_italic = true
vim.g.nord_borders = true
vim.g.nord_contrast = true

local status_ok, nord = pcall(require, "nord")
if not status_ok then
	vim.cmd([[
    colorscheme slate
]])

	return
end

nord.set()

local status_ok, lualine = pcall(require, "lualine")
if not status_ok then
	return
end

lualine.setup({
	options = {
		theme = "nord",
	},
})
