local M = {}

function M.setup()
	local safe_load = require('codethread.utils').safe_load
	safe_load('onenord', function(onenord)
		onenord.setup {
			fade_nc = false,
			styles = {
				comments = 'italic',
				strings = 'NONE',
				keywords = 'NONE',
				functions = 'NONE',
				variables = 'NONE',
				diagnostics = 'underline',
			},
			custom_highlights = vim.tbl_extend('force', {}, require 'codethread.completion.colors'), -- Overwrite default highlight groups
			custom_colors = {}, -- Overwrite default colors
		}
	end)
end

M.lualine = 'onenord'

return M
