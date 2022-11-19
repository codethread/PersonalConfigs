local navic = require 'nvim-navic'
local status_ok, lualine = pcall(require, 'lualine')
if not status_ok then
	print 'could not load lualine'
	return
end

local mode_maps = {
	['NORMAL'] = '',
	['INSERT'] = '',
	['VISUAL'] = '',
	['V-BLOCK'] = '',
	['V-LINE'] = '',
	['TERMINAL'] = '❯',
	-- ["TERMINAL"] = "",
}

local function mode_map(str)
	if mode_maps[str] == nil then return str end
	return mode_maps[str]
end

-- enable global status line
vim.opt.laststatus = 3

lualine.setup {
	options = {
		icons_enabled = true,
		theme = require('codethread.theme').lualine,
		disabled_filetypes = {},
		section_separators = { right = '', left = '' },
		component_separators = { left = '', right = '' },
	},
	sections = {
		lualine_a = {
			{
				'mode',
				icons_enabled = true,
				separator = { left = ' ', right = '' },
				fmt = mode_map,
			},
		},
		lualine_b = {
			{
				'filename',
				path = 1, -- relative path
				-- shorting_target = 40, -- leave at least 40 characters in line
				show_filename_only = false, -- can show full path with global status line
				symbols = {
					modified = '  ',
					readonly = ' ',
				},
			},
		},
		lualine_c = { 'diagnostics' },
		lualine_x = {},
		lualine_y = {
			'filetype',
			'progress',
		},
		lualine_z = {
			{
				'location',
				separator = { right = '', left = '' },
				left_padding = 2,
			},
		},
	},
	inactive_sections = {
		lualine_a = {},
		lualine_b = {},
		lualine_c = {
			{
				'filename',
				path = 1, -- relative path
				shorting_target = 40, -- leave at least 40 characters in line
				separator = { left = ' ' },
				-- right_padding = 2,
				symbols = {
					modified = '  ',
					readonly = ' ',
				},
			},
		},
		lualine_x = {
			{
				'location',
				-- separator = { right = "" },
			},
		},
		lualine_y = {},
		lualine_z = {},
	},
	tabline = {
		lualine_a = {
			{
				'tabs',
				max_length = vim.o.columns / 2,
				mode = 2, -- tab name and number
				separator = { left = ' ', right = '' },
				-- right_padding = 2,
			},
		},
		lualine_b = {
			{
				navic.get_location,
				cond = navic.is_available,
			},
		},
		lualine_c = {},
		lualine_x = {
			"%{ObsessionStatus('', '')} ",
		},
		lualine_y = { 'diff' },
		lualine_z = {
			{
				'branch',
				separator = { right = ' ', left = '' },
				-- left_padding = 2,
			},
		},
	},
	extensions = {
		'quickfix',
		'toggleterm',
		'fugitive',
	},
}
