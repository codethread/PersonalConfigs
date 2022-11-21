local M = {}

function M.setup()
	local safe_load = require('codethread.utils').safe_load
	safe_load('nord', function(nord)
		vim.g.nord_contrast = false
		vim.g.nord_borders = true
		vim.g.nord_disable_background = false
		vim.g.nord_italic = true
		vim.g.nord_uniform_diff_background = true
		vim.g.nord_bold = false

		nord.set()
	end)
end

M.lualine = 'nord'

M.setup()
return M
