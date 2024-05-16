return {
	'DanilaMihailov/beacon.nvim',
	'farmergreg/vim-lastplace',

	{
		'famiu/bufdelete.nvim',
		cmd = 'Bdelete',
	},

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
		-- event = { 'BufReadPre', 'BufNewFile' },
		event = { 'InsertEnter' },
		version = '9.*',
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
			default = {
				find = {
					options = {
						'ignore-case',
						'hidden',
					},
				},
			},
		},
	},

	{
		'windwp/nvim-autopairs',
		event = 'InsertEnter',
		config = function()
			local _, npairs = pcall(require, 'nvim-autopairs')
			npairs.setup {
				check_ts = true,
				ts_config = {
					lua = { 'string', 'source' },
					javascript = { 'string', 'template_string' },
					java = false,
				},
				disable_filetype = { 'TelescopePrompt', 'spectre_panel' },
				disable_in_macro = true,
				fast_wrap = {
					map = '<M-e>',
					chars = { '{', '[', '(', '"', "'" },
					pattern = string.gsub([[ [%'%"%)%>%]%)%}%,] ]], '%s+', ''),
					offset = 0, -- Offset from pattern match
					end_key = '$',
					keys = 'qwertyuiopzxcvbnmasdfghjkl',
					check_comma = true,
					highlight = 'PmenuSel',
					highlight_grey = 'LineNr',
				},
			}

			local cmp_autopairs = require 'nvim-autopairs.completion.cmp'
			local cmp_status_ok, cmp = pcall(require, 'cmp')
			if not cmp_status_ok then return end
			cmp.event:on('confirm_done', cmp_autopairs.on_confirm_done { map_char = { tex = '' } })
		end,
	},
}
