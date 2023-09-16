vim.cmd [[
	" center on scroll
	nnoremap <C-d> <C-d>zz
	nnoremap <C-u> <C-u>zz

	" keep cursor centered
	nnoremap n nzzzv
	nnoremap N Nzzzv
	nnoremap J mzJ`z

	" don't changed jumplist with paragraph jumps :help jumplist
	" nnoremap <silent> } :<C-u>execute "keepjumps norm! " . v:count1 . "}"<CR>
	" nnoremap <silent> { :<C-u>execute "keepjumps norm! " . v:count1 . "{"<CR>
]]

local harp = {
	{ 'ma', function() require('harpoon.mark').add_file() end, desc = 'harpoon.mark' },
	{ 'mf', '<cmd>Telescope harpoon marks<cr>', desc = '<cmd>Telescope harpoon marks<cr>' },
	{ 'ml', function() require('harpoon.ui').toggle_quick_menu() end, desc = 'harpoon.ui' },
	{ 'mk', function() require('harpoon.ui').nav_next() end, desc = 'harpoon.ui' },
	{ 'mj', function() require('harpoon.ui').nav_prev() end, desc = 'harpoon.ui' },
	{ 'mt', function() require('harpoon.term').gotoTerminal(1) end, desc = 'harpoon.term' },
}
for n = 1, 9, 1 do
	table.insert(
		harp,
		{ 'm' .. n, function() require('harpoon.ui').nav_file(n) end, desc = 'harpoon.' .. n }
	)
end

return {
	{
		'ThePrimeagen/harpoon',
		init = function() vim.keymap.set('n', 'M', 'm') end,
		keys = harp,
		opts = {
			menu = {
				width = vim.api.nvim_win_get_width(0) - 4,
			},
		},
	},

	{
		'stevearc/aerial.nvim',
		keys = {
			{ '<leader>a', Cmd 'AerialToggle! left', desc = 'Aerial' },
			-- these are reversed for my keyboard as I have them over j & k
			{ '}', Cmd 'AerialPrev', desc = 'AerialPrev' },
			{ '{', Cmd 'AerialNex', desc = 'AerialNext' },
		},
		opts = {
			layout = {
				min_width = 25,
				default_direction = 'prefer_left',
			},
			-- backends = {
			-- 	-- ['_'] = { 'lsp', 'treesitter' },
			-- 	['_'] = { 'lsp' },
			-- 	-- ['_'] = { 'treesitter' },
			-- 	python = { 'treesitter' },
			-- 	rust = { 'lsp' },
			-- },
			-- filter_kind = {
			-- 	'Class',
			-- 	'Constructor',
			-- 	'Enum',
			-- 	'Function',
			-- 	'Interface',
			-- 	'Module',
			-- 	'Method',
			-- 	'Struct',
			-- 	-- default end
			-- 	'Constant', -- want this for js, will update other things later
			-- },
		},
	},

	{
		'stevearc/oil.nvim',
		-- TODO need? 'jghauser/mkdir.nvim'
		keys = {
			{ '<C-n>', '<cmd>Oil --float<cr>', desc = 'Oil' },
			{ '<leader>od', '<cmd>Oil<cr>', desc = 'Oil' },
		},
		opts = {
			view_options = {
				show_hidden = true,
			},
			columns = {
				'icon', -- charming but as these can be edited, its annoying
				-- 'permissions',
				-- 'size',
				-- 'mtime',
			},
			skip_confirm_for_simple_edits = true,
			use_default_keymaps = true,
			keymaps = {
				['g?'] = 'actions.show_help',
				['<CR>'] = 'actions.select',
				['<C-s>'] = false,
				['<C-h>'] = false,
				['<C-t>'] = false,
				['<TAB>'] = 'actions.preview',
				['<C-c>'] = false,
				['<C-l>'] = false,
				['<left>'] = 'actions.parent',
				['<right>'] = 'actions.select',
				['_'] = false,
				['`'] = false,
				['~'] = 'actions.open_cwd',
				['g.'] = 'actions.toggle_hidden',
				['!'] = 'actions.open_cmdline',
				-- ['<localleader>c'] = 'actions.copy_entry_path', -- using which key for preview
			},
		},
		init = function()
			U.keys('oil', {
				{
					'c',
					function() require('oil.actions').copy_entry_path.callback() end,
					'copy filepath',
				},
				{ 'r', function() require('oil.actions').refresh.callback() end, 'refresh' },
				{ 't', function() require('oil.actions').select_tab.callback() end, 'open in tab' },
				{
					'v',
					function() require('oil.actions').select_vsplit.callback() end,
					'open in vertical',
				},
				{
					's',
					function() require('oil.actions').select_split.callback() end,
					'open in split',
				},
				{ '.', function() require('oil.actions').tcd.callback() end, 'make dir PWD' },
			})
		end,
	},

	{
		'folke/flash.nvim',
		event = 'VeryLazy',
		dependencies = {
			U.highlights {
				FlashLabel = { bg = 'surface', fg = 'white' },
			},
		},
		---@type Flash.Config
		opts = {
			label = {
				exclude = 'zxc',
			},
			modes = {
				char = {
					-- can set to false, but can actually just use f/F r t/T to repeat motions, in case of overshooting
					multi_line = true,
				},
			},
		},
		keys = {
			{
				's',
				mode = { 'n', 'x', 'o' },
				function()
					-- default options: exact mode, multi window, all directions, with a backdrop
					require('flash').jump()
				end,
				desc = 'Flash',
			},
			{
				'S',
				mode = { 'n', 'o', 'x' },
				function() require('flash').treesitter() end,
				desc = 'Flash Treesitter',
			},
			{
				'r',
				mode = 'o',
				function() require('flash').remote() end,
				desc = 'Remote Flash',
			},
			{
				'R',
				mode = { 'o', 'x' },
				function() require('flash').treesitter_search() end,
				desc = 'Treesitter Search',
			},
			{
				'<c-s>',
				mode = { 'c' },
				function() require('flash').toggle() end,
				desc = 'Toggle Flash Search',
			},
		},
	},
}
