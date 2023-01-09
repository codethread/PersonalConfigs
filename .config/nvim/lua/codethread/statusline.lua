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
	local hydra_status_ok, hydra = pcall(require, 'hydra.statusline')
	if not hydra_status_ok then
		if mode_maps[str] == nil then return str end
		return mode_maps[str]
	end

	if hydra.is_active() then return hydra.get_name() end
	if mode_maps[str] == nil then return str end
	return mode_maps[str]
end

local function mode_color(section)
	local errs, hydra, theme = U.requires { 'hydra.statusline', 'codethread.themes' }
	if not errs then
		if hydra.is_active() then return { bg = theme.colors().red } end
	end
end

local M = {}

function M.setup_flumpy()
	lualine.setup {
		options = {
			icons_enabled = true,
			-- theme = require('codethread.themes').lualine,
			theme = 'tokyonight',
			disabled_filetypes = {},
			section_separators = { right = '', left = '' },
			component_separators = { left = '|', right = '|' },
		},
		sections = {
			lualine_a = {
				{
					'mode',
					icons_enabled = true,
					separator = { left = ' ', right = '' },
					fmt = mode_map,
					color = mode_color,
				},
			},
			lualine_b = {
				{
					'filename',
					path = 1, -- relative path
					show_filename_only = false, -- can show full path with global status line
					shorting_target = 0, -- don't shorten the component as I'm using global status line
					symbols = {
						modified = '  ',
						readonly = ' ',
					},
				},
			},
			lualine_c = { { 'lsp_progress', 'diagnostics' } },
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
					color = mode_color,
				},
			},
		},
		tabline = {
			lualine_a = {
				{

					'tabs',
					max_length = vim.o.columns / 2,
					mode = 2, -- tab name and number
					separator = { left = ' ', right = '' },
					-- right_padding = 2,
					tabs_color = {
						active = mode_color,
						inactive = 'lualine_a_inactive',
					},
					fmt = function(name, context)
						-- Show + if buffer is modified in tab
						local buflist = vim.fn.tabpagebuflist(context.tabnr)
						local winnr = vim.fn.tabpagewinnr(context.tabnr)
						local bufnr = buflist[winnr]
						local mod = vim.fn.getbufvar(bufnr, '&mod')
						return name .. (mod == 1 and '  ' or '')
					end,
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
					color = mode_color,
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
end

return M
