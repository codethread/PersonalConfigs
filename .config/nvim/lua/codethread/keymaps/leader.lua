local fns = require 'codethread.fns'
-- TIPS
-- to see raw key
-- go to insert mode, type <C-v> then type, and that key will be shown

U.keymaps({}, {
	{ '<leader><leader>', Cmd 'Telescope find_files', 'Files' },
	{ '<leader>;', Cmd 'Telescope commands', 'M-x' },
	{ '<leader>:', Cmd 'Telescope command_history', 'M-x [hist]' },
	{ '<leader>/', Cmd 'Telescope search_history', '/ [hist]' },
	{ '<leader>,', Cmd 'Telescope resume', '🔭' },
})
require('which-key').add {
	{ '<leader>G', group = 'Global' },
	{ '<leader>T', group = 'Toggle' },
	{ '<leader>e', group = 'Errors' },
	{ '<leader>f', group = 'Buffer' },
	{ '<leader>g', group = 'Git' },
	{ '<leader>gw', group = 'Worktree' },
	{ '<leader>h', group = 'Help' },
	{ '<leader>l', group = 'LSP' },
	{ '<leader>o', group = 'Open' }, -- open, Open, openers, Openers
	{ '<leader>s', group = 'Search' },
	{ '<leader>u', group = 'Utils' },
	{ '<leader>w', group = 'Window' },
	{ '<leader>y', group = 'Yank' }, -- clipboard, copy
}
U.keymaps({}, {
	{ '<leader>eh', function() vim.diagnostic.open_float() end, 'Hover' },
	{ '<leader>el', Cmd 'Telescope diagnostics theme=ivy bufnr=0', 'Document Diagnostics' },
	{ '<leader>eL', Cmd 'Telescope diagnostics', 'Workspace Diagnostics' },
	{
		'<leader>en',
		function() require('codethread.diagnostics').next_diagnostic() end,
		'Next Diagnostic',
	},
	{
		'<leader>ep',
		function() require('codethread.diagnostics').previous_diagnostic() end,
		'Previous Diagnostic',
	},
	{ '<leader>et', Cmd 'DiagnosticToggle', 'Toggle Diagnostics' },
	{ '<leader>eq', Cmd 'lua vim.diagnostic.setloclist()', 'Quickfix' },

	{ '<leader>fb', [[<C-^>]], 'Toggle' },
	{ '<leader>fl', Cmd 'Telescope buffers', 'list' },
	{ '<leader>ff', function() require('codethread.split').split() end, 'Split last' },
	{ '<leader>fr', Cmd 'Telescope oldfiles', 'recent' },
	{ '<leader>fR', Cmd 'e!', 'reload' },
	{ '<leader>fk', function() require('mini.bufremove').delete() end, 'kill' },
	{ '<leader>fs', fns.save_buffer, 'Save' },
	{ '<leader>fu', function() require('plugins.telescope.pickers').unsaved() end, 'Unsaved' },
	{ '<leader>fO', function() Snacks.bufdelete.other {} end, 'Delete Others' },

	----
	{
		'<leader>gR',
		function()
			require('gitsigns').reset_buffer()
			vim.cmd [[noa w]]
		end,
		'Reset Buffer',
	},
	{ '<leader>gb', Cmd 'Telescope git_branches', 'Checkout branch' },
	{ '<leader>gc', Cmd 'Telescope git_commits', 'Checkout commit' },
	{ '<leader>gd', function() require('gitsigns').diffthis() end, 'Diff' },
	{ '<leader>gg', function() require('neogit').open {} end, 'Status' },
	{
		'<leader>gj',
		function() require('gitsigns').nav_hunk('next', { preview = true }) end,
		'Next Hunk',
	},
	{
		'<leader>gk',
		function() require('gitsigns').nav_hunk('prev', { preview = true }) end,
		'Prev Hunk',
	},
	{ '<leader>gh', fns.toggle_file_history, 'File History' },
	{ '<leader>gl', function() require('gitsigns').blame_line() end, 'Blame' },
	{ '<leader>go', Cmd 'Telescope git_status', 'Open changed file' },
	{ '<leader>gp', function() require('gitsigns').preview_hunk() end, 'Preview Hunk' },
	{ '<leader>gr', function() require('gitsigns').reset_hunk() end, 'Reset Hunk' },
	{ '<leader>gs', function() require('gitsigns').stage_hunk() end, '(un)Stage Hunk' },
	{ '<leader>gv', Cmd 'silent !gh repo view --web', 'Ghub view' },
	{
		'<leader>gwa',
		function() require('codethread.fns').worktree_open_alt() end,
		'Split buffer',
	},
	---
	{ '<leader>GL', Cmd 'Lazy', 'Lazy' },
	{ '<leader>GM', Cmd 'Mason', 'Mason' },
	{ '<leader>Gm', Cmd 'Bufferize messages', 'messages' },
	---
	{ '<leader>hh', Cmd 'Telescope help_tags', 'Help' },
	{ '<leader>hH', Cmd 'Telescope helpgrep', 'Help Grep' }, -- TODO: steal code and use my own grepper
	{ '<leader>hm', Cmd 'Telescope man_pages', 'Man pages' },
	{ '<leader>hv', Cmd 'Telescope vim_options', 'Settings' },
	{ '<leader>ht', Cmd 'Telescope builtin', 'Telescope' },
	{ '<leader>hc', Cmd 'Telescope highlights', 'Telescope' },
	---
	-- { '<leader>la', Cmd'Telescope lsp_code_actions them=cursor', "Code Action" },
	{ '<leader>la', Cmd 'lua vim.lsp.buf.code_action()', 'Code Action' },
	{ '<leader>ld', Cmd 'lua vim.lsp.buf.declaration({ reuse_win = true })', 'Declaration' },
	{ '<leader>li', Cmd 'LspInfo', 'Info' },
	{ '<leader>lI', Cmd 'LspInstallInfo', 'Installer Info' },
	{ '<leader>ll', Cmd 'lua vim.lsp.codelens.run()', 'CodeLens Action' },
	{ '<leader>lr', Cmd 'lua vim.lsp.buf.rename()', 'Rename' },
	{ '<leader>ls', Cmd 'Telescope lsp_document_symbols', 'Document Symbols' },
	{ '<leader>lS', Cmd 'Telescope lsp_dynamic_workspace_symbols', 'Workspace Symbols' },
	---
	{ '<leader>wN', Cmd 'tabnew', 'New Tab' },
	{ '<leader>wk', Cmd 'close', 'Close' },
	{ '<leader>wl', function() require('telescope-tabs').list_tabs() end, 'List Tabs' }, -- TODO: put through telescope
	{ '<leader>wn', Cmd 'tabNext', 'Next Tab' }, -- TODO: put through telescope
	{ '<leader>wp', Cmd 'tabprevious', 'Previous Tab' }, -- TODO: put through telescope
	{ '<leader>ww', Cmd 'vsplit', 'Split' }, -- TODO: put through telescope
	{ '<leader>ws', Cmd 'SwapSplit', 'Swap' },
	{
		'<leader>we',
		function() require('codethread.movement').tab_hydra:activate() end,
		'🐉 Tabs',
	},
	---
	{ '<leader>sb', Cmd 'Telescope git_branches', 'Checkout branch' },
	{ '<leader>sc', Cmd 'Telescope colorscheme', 'Colorscheme' },
	{ '<leader>sf', Cmd 'Telescope current_buffer_fuzzy_find mirror=true', 'Buffer' },
	{
		'<leader>sF',
		function()
			require('telescope.builtin').live_grep {
				grep_open_files = true,
			}
		end,
		'Buffers',
	},
	{ '<leader>sr', Cmd 'Telescope oldfiles', 'Open Recent File' },
	{ '<leader>sR', Cmd 'Telescope registers', 'Registers' },
	{ '<leader>sk', Cmd 'Telescope keymaps', 'Keymaps' },
	{ '<leader>sm', function() require('plugins.telescope.pickers').multi_grep {} end, 'Multi-grep' },
	{ '<leader>sC', Cmd 'Telescope commands', 'Commands' },
	-- { '<leader>sp', Cmd 'Telescope live_grep_args', 'Live Grep' },
	{
		'<leader>sp',
		function() require('plugins.telescope.pickers.rg').live_grepper {} end,
		'Live Grep',
	},
	{
		'<leader>sw',
		function()
			require('telescope-live-grep-args.shortcuts').grep_word_under_cursor {
				-- TODO fix for being in a nested dir
				cwd = vim.fs.root(0, '.git'),
			}
		end,
		'Word',
	},
	{ '<leader>sl', Cmd 'Telescope lsp_document_symbols', 'Symbol' },
	{ '<leader>sL', ':Telescope lsp_workspace_symbols query=', 'Global Symbols' },
	{ '<leader>ss', Cmd 'Spectre', 'Spectre' },
	{ '<leader>sy', Cmd 'Telescope neoclip', 'Clipboard' },
	---
	{ '<leader>oa', Cmd 'AerialToggle! left', 'Aerial' },
	{ '<leader>of', function() vim.ui.open(vim.fn.expand '%:p:h') end, 'Open finder' },
	-- if you can't beat 'em
	{ '<leader>oc', Cmd 'silent !code %', 'VSCode' },
	{ '<leader>od', Cmd 'Oil', 'Dir' },
	{ '<leader>ol', function() require('codethread.logger').select() end, 'open log file' },
	{ '<leader>on', Cmd 'LogOpen notifications', 'Notifcation History' },
	{ '<leader>oN', function() Snacks.notifier.hide() end, 'Hide Notifications' },
	---
	{ '<leader>Ti', Cmd "lua print'nothing setup'", 'Inlay Hints' },
	{ '<leader>T ', fns.toggle_listchars, 'Whitespace' },
	{ '<leader>T>', fns.toggle_indent_scope, 'IndentScope' },

	---
	{ '<leader>uf', "mbggVG=='b", 'format buffer' },
	{ '<leader>us', '1z=', 'autocorrect' },
	{ '<leader>ub', function() require('codethread.box').box() end, 'Box' },
	{
		'<leader>ur',
		function()
			require('luasnip.loaders.from_vscode').load {
				paths = {
					'~/.config/nvim/snippets_vscode',
					'~/.local/share/nvim/lazy/friendly-snippets',
				},
			}
		end,
		'reload snippets',
	},
	---
	{ '<leader>yl', fns.copy_filepath_relative, 'path [gitlab]' },
	{ '<leader>yr', Cmd 'let @*=@%', 'path [root]' }, -- TODO:
	{ '<leader>yy', Cmd 'let @*=@%', 'path [relative]' }, -- TODO:
	{ '<leader>ys', Cmd 'Telescope neoclip', 'Search' },
})

return {
	-- ['<leader>v'] = { name = '+vim' },

	-- a = { Cmd 'Other', 'other' },

	-- b = {
	-- 	name = '...',
	-- },

	-- d = {
	--   function()
	--     -- TODO: potentiall save all buffers first
	--     local dap = require 'dap'
	--     if dap.session() then
	--       require('codethread.lsp.dap-hydra').debug_running_hydra:activate()
	--     else
	--       require('codethread.lsp.dap-hydra').debug_hydra:activate()
	--     end
	--   end,
	--   '🐉 Debug',
	-- },

	-- files, buffers, buffer

	j = {
		name = 'Test',
		j = { fns.test_current_file, 'file' },
	},

	['m'] = {
		function() require('codethread.movement').mover_hydra:activate() end,
		'🐉 Mover',
	},

	-- NOTE: this is a terrible keymap
	k = {
		function() require('codethread.fold').fold_hyrda:activate() end,
		'🐉 Folds',
	},

	W = { Cmd 'Maximize', 'Window Maximise' },

	n = {
		name = 'notes',
		n = { Cmd 'vsplit brain.md', 'open' },
		-- a = {
		-- 	name = 'add',
		-- 	a = { Cmd 'ObsidianNew', 'New' },
		-- 	t = { function() require('plugins.notes.fns').create_markdown_toc() end, 'ToC' },
		-- },
		-- b = { Cmd 'ObsidianBacklinks', 'Backlinks' },
		-- i = { Cmd 'ObsidianTemplate', 'Template' },
		-- l = { Cmd 'ObsidianLinks', 'Links' },
		-- n = { Cmd 'ObsidianQuickSwitch', 'Find' },
		-- o = {
		-- 	name = 'open',
		-- 	o = { Cmd 'ObsidianOpen', 'Obsidian' },
		-- 	t = { Cmd 'ObsidianToday', 'Today' },
		-- 	h = { function() require('plugins.notes.fns').notes_path_to '!home.md' end, 'Home' },
		-- 	p = {
		-- 		function() require('plugins.notes.fns').notes_path_to '!projects.md' end,
		-- 		'Projects',
		-- 	},
		-- 	q = { function() require('plugins.notes.fns').notes_path_to 'dump.md' end, 'Dump' },
		-- },
		-- q = {
		-- 	--- TODO: make this more like an input simpilar to emacs
		-- 	function() require('plugins.notes.fns').notes_path_to 'dump.md' end,
		-- 	'quick capture',
		-- },
		-- r = { function() require('plugins.notes.fns').rename() end, 'Rename' },
		-- s = { Cmd 'ObsidianSearch', 'Search' },
		-- t = { Cmd 'ObsidianTags', 'Tags' },
	},

	t = {
		name = 'Terminal',
		n = { Cmd 'lua _NODE_TOGGLE()', 'Node' },
		t = { Cmd 'ToggleTerm direction=float', 'Float' },
		h = { Cmd 'ToggleTerm size=10 direction=horizontal', 'Horizontal' },
		v = { Cmd 'ToggleTerm size=80 direction=vertical', 'Vertical' },
	},
}
