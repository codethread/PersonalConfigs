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

local function mode_color()
	local hydra, ok = require 'hydra.statusline'
	if not ok then return end

	if hydra.is_active() then return { bg = 'love' } end
end

return {
	{
		'nvim-lualine/lualine.nvim',
		dependencies = {
			-- 'arkav/lualine-lsp-progress',
			{
				'SmiteshP/nvim-navic',
				lazy = true,
				init = function()
					-- vim.g.navic_silence = true
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
						depth_limit = 5,
						-- icons = require('lspkind').symbol_map,
					}
				end,
			},
		},
		event = 'VeryLazy',
		opts = {
			options = {
				theme = 'auto',
				icons_enabled = true,
				globalstatus = true,
				disabled_filetypes = {
					statusline = { 'dashboard', 'alpha' },
					winbar = {
						'oil',
						'help',
						'startify',
						'dashboard',
						'packer',
						'neogitstatus',
						'NvimTree',
						'Trouble',
						'alpha',
						'lir',
						'Outline',
						'spectre_panel',
						'toggleterm',
						'qf',

						'harpoon',

						-- dapui
						'dapui_config',
						'dapui_watches',
						'dapui_stacks',
						'dapui_breakpoints',
						'dapui_scopes',
						'dapui_config',
						'dapui_console',
						'dapui_hover',
						'dapui_repl',
						'dapui_controls',
						'dapui_state',

						-- dap
						'dap-repl',
						'dap-hover',
					},
				},
				section_separators = { right = '', left = '' },
				component_separators = { left = '|', right = '|' },
				refresh = {
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
						separator = { left = ' ', right = '' },
						fmt = mode_map,
						color = mode_color,
					},
				},
				lualine_b = {
					{
						'branch',
						separator = { right = ' ', left = '' },
						-- left_padding = 2,
					},
					{
						'filename',
						path = 1,
						show_filename_only = false,
						shorting_target = 0,
						symbols = {
							modified = '  ',
							readonly = ' ',
						},
					},
				},
				lualine_c = {
					'diagnostics',
					{
						function() return require('nvim-navic').get_location() end,
						cond = function()
							return package.loaded['nvim-navic']
								and require('nvim-navic').is_available()
						end,
					},
				},
				lualine_x = {
					{
						function() return '  ' .. require('dap').status() end,
						cond = function()
							return package.loaded['dap'] and require('dap').status() ~= ''
						end,
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
			winbar = {
				lualine_c = {
					{
						'filename',
						path = 0,
						show_filename_only = false,
						shorting_target = 0,
						symbols = nil,
					},
				},
			},
			inactive_winbar = {
				lualine_c = {
					{
						'filename',
						path = 0,
						show_filename_only = false,
						shorting_target = 0,
						symbols = nil,
					},
				},
			},
			tabline = {
				lualine_b = {
					{
						'tabs',
						max_length = vim.o.columns / 2,
						mode = 1, -- realtive name
						-- mode = 2, -- tab name and number
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
				lualine_x = {
					"%{ObsessionStatus('', '')} ",
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
			},
		},
	},
}
