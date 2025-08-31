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
			hooks = {
				-- TODO: not quite working
				-- This hook is called whenever a file is about to be opened.
				---@param filename (string) the full-path of the file
				---@param exists (boolean) doess the file already exist
				---@return (boolean) When true (default) the plugin takes care of opening the file, when the function returns false this indicated that opening of the file is done in the hook.
				onOpenFile = function(filename, exists)
					if not vim.g.vscode then return true end
					local vscode = require 'vscode'
					vscode.action('vscode.open', { uri = filename })
					return
				end,
			},
		},
	},

	{
		'ThePrimeagen/harpoon',
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
				-- placement = 'edge',
			},
			close_automatic_events = { 'unfocus', 'switch_buffer' },

			manage_folds = false,
			link_folds_to_tree = false,
			link_tree_to_folds = false,
			on_attach = function(b) require('aerial').tree_close_all(b) end,
		},
	},

	{
		'stevearc/oil.nvim',
		cmd = 'Oil',
		opts = {
			default_file_explorer = true,
			view_options = {
				show_hidden = true,
			},
			columns = {
				'icon',
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
		{ folke / flash.nvim },
		{},
		{},
		{},
		{},
		{},
		{},
		{},
	},
}
