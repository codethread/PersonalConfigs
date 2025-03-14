return {
	'farmergreg/vim-lastplace',
	'anuvyklack/hydra.nvim',

	{ 'echasnovski/mini.bufremove', lazy = true },

	{
		'mbbill/undotree',
		init = function()
			vim.cmd [[
				" add undo break points on key stroke to make undo more granular
				inoremap , ,<c-g>u
				inoremap . .<c-g>u
				inoremap ( (<c-g>u
				inoremap { {<c-g>u
				]]

			vim.keymap.set('n', 'U', Cmd 'UndotreeToggle', {})
			vim.opt.swapfile = false
			vim.opt.backup = false
			vim.opt.writebackup = false -- This is recommended by coc

			vim.cmd [[
			if has("persistent_undo")
			let target_path = expand('~/.local/share/nvim/undodir')

			" create the directory and any parent directories
			" if the location does not exist.
			if !isdirectory(target_path)
			call mkdir(target_path, "p", 0700)
			endif

			let &undodir=target_path
			set undofile
			endif
			]]
		end,
	},

	{
		'wakatime/vim-wakatime',
		cond = os.getenv 'WAKATIME_HOME' ~= nil,
		lazy = false,
		-- event = { 'BufReadPre', 'BufNewFile' },
		-- event = { 'InsertEnter' },
	},

	{
		'AndrewRadev/bufferize.vim',
		cmd = 'Bufferize',
	},

	{
		-- store clipboard for easy recall
		'AckslD/nvim-neoclip.lua',
		event = 'TextYankPost',
		config = function()
			require('neoclip').setup()
			require('telescope').load_extension 'neoclip'
		end,
	},

	{ -- find/replace
		'windwp/nvim-spectre',
		build = 'brew install gnu-sed',
		cmd = 'Spectre',
		opts = {
			find_engine = {
				rg = {
					args = {
						'--color=never',
						'--no-heading',
						'--with-filename',
						'--line-number',
						'--glob',
						'!.git',
						'--column',
					},
				},
			},
			default = {
				find = {
					options = {
						'ignore-case',
						'hidden',
					},
				},
				replace = {
					cmd = 'sd',
				},
			},
		},
	},

	{
		'folke/which-key.nvim',
		lazy = false,
		priority = 100,
		opts = {
			plugins = {
				-- presets = {
				-- 	operators = false,
				-- 	motions = false,
				-- 	text_objects = false,
				-- },
			},
			icons = {
				mappings = false, -- turns of icons
			},
			sort = { 'order' },
			expand = 1,
		},
	},

	-- Lua
	{
		'folke/persistence.nvim',
		event = 'BufReadPre', -- this will only start session saving when an actual file was opened
		opts = {
			branch = false,
		},
	},
}
