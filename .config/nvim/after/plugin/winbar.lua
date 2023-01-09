local winbar_status_ok, winbar = pcall(require, 'winbar')
if not winbar_status_ok then
	print 'could not load winbar'
	return
end

winbar.setup {
	enabled = true,

	show_file_path = true,
	show_symbols = true,

	colors = {
		path = '', -- You can customize colors like #c946fd
		file_name = '',
		symbols = '',
	},

	icons = {
		file_icon_default = '',
		seperator = '>',
		editor_state = '●',
		lock_icon = '',
	},

	exclude_filetype = {
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
}
