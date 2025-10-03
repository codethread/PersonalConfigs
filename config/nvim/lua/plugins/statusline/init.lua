if vim.g.vscode then return {} end

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

			local utils = require 'plugins.statusline.filename'

			return {
				options = {
					theme = utils.theme(),
					icons_enabled = true,
					globalstatus = true,
					disabled_filetypes = {
						statusline = { 'snacks_dashboard' },
						tabline = { 'snacks_dashboard' },
						winbar = {
							'snacks_dashboard',
							'help',
							'no-neck-pain',
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
						{ 'mode', icons_enabled = true, fmt = utils.mode_map },
					},
					lualine_b = {
						{ 'branch' },
						'%=',
						{ utils.update_status },
					},
					lualine_c = {},
					lualine_x = {
						{
							function() return '🎤 Recording...' end,
							cond = function() return _G.claude_recording_active == true end,
						},
						{
							function() return '📝 Transcribing...' end,
							cond = function() return _G.claude_transcription_active == true end,
						},
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
					lualine_a = {
						{ 'diagnostics' },
					},
					lualine_b = {
						{ utils.filename_winbar },
					},
					lualine_c = {
						{
							function() return require('nvim-navic').get_location() end,
							cond = function()
								return package.loaded['nvim-navic'] and require('nvim-navic').is_available()
							end,
						},
					},
				},
				inactive_winbar = {
					lualine_c = { { 'diagnostics' }, { utils.filename_winbar } },
				},
				tabline = {
					lualine_a = {
						{
							'tabs',
							max_length = vim.o.columns / 2,
							mode = 1, -- realtive name
							-- mode = 2, -- tab name and number
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
						{ utils.unsaved_buffers },
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
