local mode_maps = {
	['INSERT'] = '',
	['NORMAL'] = '',
	['VISUAL'] = '',
	['V-BLOCK'] = '',
	['V-LINE'] = '',
	['TERMINAL'] = '❯',
	-- ["TERMINAL"] = "",
}

local function mode_map(str)
	if mode_maps[str] == nil then return str end
	return mode_maps[str]

	-- local hydra_status_ok, hydra = pcall(require, 'hydra.statusline')
	-- if not hydra_status_ok then
	-- 	if mode_maps[str] == nil then return str end
	-- 	return mode_maps[str]
	-- end
	--
	-- if hydra.is_active() then return hydra.get_name() end
	-- if mode_maps[str] == nil then return str end
	-- return mode_maps[str]
end

if vim.g.vscode then return {} end
local function theme()
	-- local theme = require 'lualine.themes.rose-pine-alt'
	local p = require 'rose-pine.palette'
	local config = require 'rose-pine.config'

	local bg_base = 'NONE'
	local bg = 'NONE' or p.surface
	-- theme.normal.c.gui = 'none'
	-- theme.normal.c.gui = 'none'

	return {
		normal = {
			a = { bg = bg, fg = p.rose, gui = 'bold' },
			b = { bg = bg, fg = p.text },
			c = { bg = bg, fg = p.subtle, gui = 'italic' },
		},
		insert = {
			a = { bg = bg, fg = p.foam, gui = 'bold' },
		},
		visual = {
			a = { bg = bg, fg = p.iris, gui = 'bold' },
		},
		replace = {
			a = { bg = bg, fg = p.pine, gui = 'bold' },
		},
		command = {
			a = { bg = bg, fg = p.love, gui = 'bold' },
		},
		inactive = {
			a = { bg = bg_base, fg = p.subtle, gui = 'bold' },
			b = { bg = bg_base, fg = p.subtle },
			c = { bg = bg_base, fg = p.subtle, gui = 'italic' },
		},
		inactive_winbar = {
			a = { bg = bg_base, fg = p.pine, gui = 'bold' },
			b = { bg = bg_base, fg = p.pine },
			c = { bg = bg_base, fg = p.pine, gui = 'italic' },
		},
		winbar = {
			a = { bg = bg_base, fg = p.pine, gui = 'bold' },
			b = { bg = bg_base, fg = p.pine },
			c = { bg = bg_base, fg = p.pine, gui = 'italic' },
		},
	}
end

return {
	{
		'SmiteshP/nvim-navic',
		dependencies = {
			'onsails/lspkind.nvim',
		},
		init = function()
			U.lsp_attach('*', function(client, buffer)
				if client.server_capabilities.documentSymbolProvider then
					require('nvim-navic').attach(client, buffer)
				end
			end)
		end,
		opts = function()
			return {
				-- separator = ' ',
				highlight = true,
				-- depth_limit = 5,
				icons = require('lspkind').symbol_map,
			}
		end,
	},

	{
		'nvim-lualine/lualine.nvim',
		event = 'VeryLazy',
		opts = function()
			-- use a refresh on specific events rather than lualines default polling
			vim.defer_fn(function()
				local ll = require 'lualine'
				vim.api.nvim_create_autocmd({ 'CursorHold', 'BufEnter' }, {
					callback = function() ll.refresh() end,
				})
			end, 1000)

			local components = {
				left = '',
				right = '',
				-- section_separators = { right = '', left = '' },
			}

			local base_statusline_highlights = {
				'StatusLine',
				'StatusLineNC',
				'Tabline',
				'TabLineFill',
				'TabLineSel',
				'Winbar',
				'WinbarNC',
			}
			for _, hl_group in pairs(base_statusline_highlights) do
				vim.api.nvim_set_hl(0, hl_group, { bg = 'none' })
			end

			return {
				options = {
					theme = theme(),
					icons_enabled = true,
					globalstatus = true,
					disabled_filetypes = {
						-- statusline = { 'alpha' },
						winbar = {
							'help',
							'no-neck-pain',
							'dashboard',
							'neogitstatus',
							'Outline',
						},
					},
					section_separators = { right = '', left = '' },
					component_separators = { left = '', right = '' },
					refresh = {
						-- see events in `init`
						statusline = 10000,
						tabline = 10000,
						winbar = 10000,
					},
				},
				sections = {
					lualine_a = {
						{
							'mode',
							icons_enabled = true,
							-- separator = { left = ' ', right = '' },
							fmt = mode_map,
						},
					},
					lualine_b = {
						{
							'branch',
							-- separator = { right = ' ', left = '' },
							-- left_padding = 2,
						},
						'%=',
						{
							require('plugins.statusline.filename').update_status,
						},
					},
					lualine_x = {
						{
							function() return '  ' .. require('dap').status() end,
							cond = function() return package.loaded['dap'] and require('dap').status() ~= '' end,
							-- color = Util.fg("Debug"),
						},
						{
							require('lazy.status').updates,
							cond = require('lazy.status').has_updates,
							-- color = Util.fg 'Special',
						},
					},
					lualine_y = {
						'filetype',
						'progress',
						-- 'filesize',
					},
					lualine_z = {
						{
							'location',
							-- separator = { right = '', left = '' },
							left_padding = 2,
						},
					},
				},
				winbar = {
					lualine_c = {
						{ 'diagnostics' },
						{ require('plugins.statusline.filename').filename_winbar },
						{
							function() return require('nvim-navic').get_location() end,
							cond = function()
								return package.loaded['nvim-navic'] and require('nvim-navic').is_available()
							end,
						},
					},
				},
				inactive_winbar = {
					lualine_c = {
						{ 'diagnostics' },
						{ require('plugins.statusline.filename').filename_winbar },
					},
				},
				tabline = {
					lualine_a = {
						{
							'tabs',
							max_length = vim.o.columns / 2,
							mode = 1, -- realtive name
							-- mode = 2, -- tab name and number
							-- separator = { left = ' ', right = '' },
							-- right_padding = 2,
							tabs_color = {
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
					lualine_z = {
						{
							function()
								for _, buf in ipairs(vim.api.nvim_list_bufs()) do
									local is_modified = vim.api.nvim_buf_get_option(buf, 'modified')
									local cur = vim.api.nvim_get_current_buf()
									if is_modified and cur ~= buf then
										return 'Unsaved buffers' -- any message or icon
									end
								end
								return ''
							end,
						},
					},
				},
				extensions = {
					'quickfix',
					'toggleterm',
					'fugitive',
					'lazy',
					'oil',
					'nvim-dap-ui',
					'mason',
					'trouble',
				},
			}
		end,
	},
}
