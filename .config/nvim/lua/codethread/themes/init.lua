-- local nord = require 'codethread.themes.nord'
local tokyo = require 'codethread.themes.tokyonight'

require('codethread.themes.setup').setup(tokyo)

return {
	lualine = tokyo.statusline,
	colors = tokyo.colors,
}
