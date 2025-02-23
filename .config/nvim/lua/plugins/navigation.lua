return {
	{
		'andymass/vim-matchup',
		keys = { { '%' }, { 'g%' }, { '[%' }, { ']%' }, { 'z%' } },
		init = function() vim.g.matchup_matchparen_offscreen = { method = 'popup' } end,
	},

	{
		'rgroli/other.nvim',
		name = 'other-nvim',
		cmd = 'Other',
		opts = {
			mappings = {
				{ pattern = 'lua/(.*)/(.*).lua$', target = 'test/%1/%2.lua' },
				{ pattern = 'test/(.*)/(.*).lua$', target = 'lua/%1/%2.lua' },

				{
					---@param path string
					---@return string[]?
					pattern = function(path)
						local stem, ext = path:match '(.*)%.test%.(m?[tj]sx?)$'
						if stem then return { stem, ext } end
						stem, ext = path:match '(.*)%.(m?[tj]sx?)$'
						return { stem .. '.test', ext }
					end,
					target = '%1.%2',
				},
			},
		},
	},

	{
		'ThePrimeagen/harpoon',
		lazy = true,
		opts = {
			menu = {
				width = vim.api.nvim_win_get_width(0) - 4,
			},
		},
	},

	{
		'stevearc/aerial.nvim',
		cmd = { 'AerialToggle', 'AerialNext', 'AerialPrev' },
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
		dependencies = { 'nvim-tree/nvim-web-devicons' },
		cmd = 'Oil',
		opts = {
			default_file_explorer = true,
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
			lsp_file_methods = {
				-- Set to true to autosave buffers that are updated with LSP willRenameFiles
				-- Set to "unmodified" to only save unmodified buffers
				-- NOTE: watch out for this during refactors
				autosave_changes = true,
			},
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
			--[[stylua: ignore]] --format
			Keys.localleader_ft('oil', {
	{ 'c', 'copy filepath'   , function() require('oil.actions').copy_entry_path.callback() end },
	{ 'r', 'refresh'         , function() require('oil.actions').refresh.callback() end         },
	{ 't', 'open in tab'     , function() require('oil.actions').select_tab.callback() end      },
	{ 'v', 'open in vertical', function() require('oil.actions').select_vsplit.callback() end   },
	{ 's', 'open in split'   , function() require('oil.actions').select_split.callback() end    },
	{ '.', 'make dir PWD'    , function() require('oil.actions').tcd.callback() end             },
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
				exclude = 'xb',
			},
			search = {
				multi_window = false,
			},
			modes = {
				search = {
					enabled = false, -- i do like this but it's annoying on large files
				},
				char = {
					-- can set to false, but can actually just use f/F r t/T to repeat motions, in case of overshooting
					multi_line = true,
				},
			},
		},
		keys = {
			{
				'<C-f>',
				mode = { 'n', 'x', 'o' },
				function()
					-- default options: exact mode, multi window, all directions, with a backdrop
					require('flash').jump()
				end,
				desc = 'Flash',
			},
			-- TODO: disable for now while playing with surround stuff
			-- 	{
			-- 		'S',
			-- 		mode = { 'n', 'o', 'x' },
			-- 		-- function() require('flash').treesitter() end,
			-- 		function() require('flash').jump { search = { multi_window = true } } end,
			-- 		desc = 'Flash Treesitter',
			-- 	},
			-- 	{
			-- 		'r',
			-- 		mode = 'o',
			-- 		function() require('flash').remote() end,
			-- 		desc = 'Remote Flash',
			-- 	},
			-- 	{
			-- 		'R',
			-- 		mode = { 'o', 'x' },
			-- 		function() require('flash').treesitter_search() end,
			-- 		desc = 'Treesitter Search',
			-- 	},
			-- 	{
			-- 		'<c-s>',
			-- 		mode = { 'c' },
			-- 		function() require('flash').toggle() end,
			-- 		desc = 'Toggle Flash Search',
			-- 	},
		},
	},
}
