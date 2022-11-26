local safe_load = require('codethread.utils').safe_load

local M = {}

function M.colors()
	-- TODO make this easier to consume with some types perhaps??
	return safe_load('nord', function()
		local t = require 'nord.colors'
		local c = require 'nord.named_colors'
		local dark = '#242933'

		return {
			t = t,
			c = c,
			dark = dark,
		}
	end)
end

function M.setup()
	safe_load('nord', function(nord)
		vim.g.nord_contrast = true
		vim.g.nord_borders = true
		vim.g.nord_disable_background = false
		vim.g.nord_italic = true
		vim.g.nord_uniform_diff_background = true
		vim.g.nord_bold = false

		local theme = M.colors() or {}
		local t = theme.t
		local c = theme.c
		local dark = theme.dark
		local utils = require 'codethread.utils'
		local hl = utils.hl

		t.float = theme.dark
		nord.set()

		local colors = {
			LineNr = { fg = c.teal },
			-- FoldColumn = { fg = c.dark_gray },
			NormalDark = { bg = dark },
			NvimTreeNormal = { bg = dark },
			TelescopeSelection = { bg = t.nord1_gui },
			['@keyword.return'] = { fg = c.orange },
		}

		hl(colors)

		vim.cmd [[
            augroup NordHighlights
                au!
                au FileType help setlocal winhighlight=Normal:NormalDark,SignColumn:NormalDark
            augroup end
        ]]
	end)
end

M.lualine = 'nord'

-- M.setup()
return M
