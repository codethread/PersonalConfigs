local require = require('codethread.utils').require

local navic, n_ok = require 'nvim-navic'
local lualine, s_ok = require 'lualine'
local th, t_ok = require 'codethread.themes'
if not n_ok or not s_ok or not t_ok then return end
local on_change = th.on_change

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

---@param theme ColorScheme
---@return function(section: string): nil
local function mode_color_hof(theme)
	return function()
		local hydra, ok = require 'hydra.statusline'
		if not ok then return end

		if hydra.is_active() then return { bg = theme.red } end
	end
end

on_change(function(_, colors)
	local mode_color = mode_color_hof(colors)

	lualine.setup {
		options = {
			icons_enabled = true,
			theme = th.lualine,
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
				{
					'branch',
					separator = { right = ' ', left = '' },
					-- left_padding = 2,
				},
			},
			lualine_b = {
				{
					'filename',
					path = 0,
					show_filename_only = false,
					shorting_target = 0,
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
					-- fmt = function(name, context)
					-- 	-- Show + if buffer is modified in tab
					-- 	local buflist = vim.fn.tabpagebuflist(context.tabnr)
					-- 	local winnr = vim.fn.tabpagewinnr(context.tabnr)
					-- 	local bufnr = buflist[winnr]
					-- 	local mod = vim.fn.getbufvar(bufnr, '&mod')
					-- 	return name .. (mod == 1 and '  ' or '')
					-- end,
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
			-- lualine_z = {
			-- 	{
			-- 		'branch',
			-- 		separator = { right = ' ', left = '' },
			-- 		-- left_padding = 2,
			-- 	},
			-- },
		},
		extensions = {
			'quickfix',
			'toggleterm',
			'fugitive',
		},
	}
end)
