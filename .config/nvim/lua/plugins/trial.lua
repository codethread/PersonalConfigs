return {
	-- TODO unconfigured
	'anuvyklack/hydra.nvim',
	'tpope/vim-eunuch', -- unix helpers, :Rename, :Delete
	'tpope/vim-obsession',
	'wellle/targets.vim',
	{
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
	}, -- find/replace
	{
		-- TODO not working??
		'telescope.nvim',
		dependencies = {
			'AckslD/nvim-neoclip.lua',
			keys = {
				{ '<leader>sy', Cmd 'Telescope neoclip', desc = 'Neoclip' },
			},
			config = function()
				require('neoclip').setup()
				require('telescope').load_extension 'neoclip'
			end,
		},
	},
	'farmergreg/vim-lastplace',
	'nvim-lua/popup.nvim',
	'DanilaMihailov/beacon.nvim',

	-- use { 'bennypowers/nvim-regexplainer', requires = 'MunifTanjim/nui.nvim' }
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
