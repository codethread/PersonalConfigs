local fns = require 'codethread.fns'
-- TIPS
-- to see raw key
-- go to insert mode, type <C-v> then type, and that key will be shown

return {
	['<leader>'] = { Cmd 'Telescope find_files', 'Files' },
	[']'] = { name = '+next' },
	['['] = { name = '+prev' },
	-- ['<leader>v'] = { name = '+vim' },
	[';'] = { Cmd 'Telescope commands', 'M-x' },
	[':'] = { Cmd 'Telescope command_history', 'M-x [hist]' },
	['/'] = { Cmd 'Telescope search_history', '/ [hist]' },
	['q'] = { Cmd 'w | luafile %', 'Reload Luafile' },
	[','] = { Cmd 'Telescope resume', '🔭' },

	a = { Cmd 'Other', 'other' },

	-- b = {
	-- 	name = '...',
	-- },

	e = {
		name = 'Errors',
		h = { function() vim.diagnostic.open_float() end, 'Hover' },
		l = { Cmd 'Telescope diagnostics theme=ivy bufnr=0', 'Document Diagnostics' },
		L = { Cmd 'Telescope diagnostics', 'Workspace Diagnostics' },
		n = {
			function()
				local ft = vim.bo.filetype
				vim.diagnostic.jump {
					severity = ft ~= 'lua' and vim.diagnostic.severity.ERROR or vim.diagnostic.severity.WARN,
					count = 1,
					float = true,
				}
			end,
			'Next Diagnostic',
		},
		p = {
			function()
				local ft = vim.bo.filetype
				vim.diagnostic.jump {
					severity = ft ~= 'lua' and vim.diagnostic.severity.ERROR or vim.diagnostic.severity.WARN,
					count = -1,
					float = true,
				}
			end,
			'Previous Diagnostic',
		},
		t = { Cmd 'DiagnosticToggle', 'Toggle Diagnostics' },
		q = { Cmd 'lua vim.diagnostic.setloclist()', 'Quickfix' },
	},

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
	f = {
		name = 'Buffer',
		b = { [[<C-^>]], 'Toggle' },
		l = { Cmd 'Telescope buffers', 'list' },
		f = { function() require('codethread.split').split() end, 'Split last' },
		r = { Cmd 'Telescope oldfiles', 'recent' },
		R = { Cmd 'e!', 'reload' },
		k = { function() require('mini.bufremove').delete() end, 'kill' },
		s = { fns.save_buffer, 'Save' },
		u = { function() require('plugins.telescope.pickers').unsaved() end, 'Unsaved' },
		O = { function() Snacks.bufdelete.other {} end, 'Delete Others' },
	},

	j = {
		name = 'Test',
		j = { fns.test_current_file, 'file' },
	},

	g = {
		name = 'Git',
		R = {
			function()
				require('gitsigns').reset_buffer()
				vim.cmd [[noa w]]
			end,
			'Reset Buffer',
		},
		b = { Cmd 'Telescope git_branches', 'Checkout branch' },
		c = { Cmd 'Telescope git_commits', 'Checkout commit' },
		d = { function() require('gitsigns').diffthis() end, 'Diff' },
		g = { function() require('neogit').open {} end, 'Status' },
		j = { function() require('gitsigns').nav_hunk('next', { preview = true }) end, 'Next Hunk' },
		k = { function() require('gitsigns').nav_hunk('prev', { preview = true }) end, 'Prev Hunk' },
		h = { fns.toggle_file_history, 'File History' },
		l = { function() require('gitsigns').blame_line() end, 'Blame' },
		o = { Cmd 'Telescope git_status', 'Open changed file' },
		p = { function() require('gitsigns').preview_hunk() end, 'Preview Hunk' },
		r = { function() require('gitsigns').reset_hunk() end, 'Reset Hunk' },
		s = { function() require('gitsigns').stage_hunk() end, 'Stage Hunk' },
		u = { function() require('gitsigns').undo_stage_hunk() end, 'Undo Stage Hunk' },
		v = { Cmd 'silent !gh repo view --web', 'Ghub view' },
	},

	G = {
		name = 'Global',
		L = { Cmd 'Lazy', 'Lazy' },
		M = { Cmd 'Mason', 'Mason' },
		m = { Cmd 'Bufferize messages', 'messages' },
	},

	h = {
		name = 'Help',
		h = { Cmd 'Telescope help_tags', 'Help' },
		H = { Cmd 'Telescope helpgrep', 'Help Grep' }, -- TODO: steal code and use my own grepper
		m = { Cmd 'Telescope man_pages', 'Man pages' },
		v = { Cmd 'Telescope vim_options', 'Settings' },
		t = { Cmd 'Telescope builtin', 'Telescope' },
		c = { Cmd 'Telescope highlights', 'Telescope' },
	},

	l = {
		name = 'LSP',
		-- a = { Cmd'Telescope lsp_code_actions them=cursor', "Code Action" },
		a = { Cmd 'lua vim.lsp.buf.code_action()', 'Code Action' },
		d = { Cmd 'lua vim.lsp.buf.declaration({ reuse_win = true })', 'Declaration' },
		i = { Cmd 'LspInfo', 'Info' },
		I = { Cmd 'LspInstallInfo', 'Installer Info' },
		l = { Cmd 'lua vim.lsp.codelens.run()', 'CodeLens Action' },
		r = { Cmd 'lua vim.lsp.buf.rename()', 'Rename' },
		s = { Cmd 'Telescope lsp_document_symbols', 'Document Symbols' },
		S = { Cmd 'Telescope lsp_dynamic_workspace_symbols', 'Workspace Symbols' },
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

	w = {
		name = 'Window',
		N = { Cmd 'tabnew', 'New Tab' },
		k = { Cmd 'close', 'Close' },
		l = { function() require('telescope-tabs').list_tabs() end, 'List Tabs' }, -- TODO: put through telescope
		n = { Cmd 'tabNext', 'Next Tab' }, -- TODO: put through telescope
		p = { Cmd 'tabprevious', 'Previous Tab' }, -- TODO: put through telescope
		w = { Cmd 'vsplit', 'Split' }, -- TODO: put through telescope
		s = { Cmd 'SwapSplit', 'Swap' },
		e = {
			function() require('codethread.movement').tab_hydra:activate() end,
			'🐉 Tabs',
		},
	},
	W = { Cmd 'Maximize', 'Window Maximise' },

	s = {
		name = 'Search',
		b = { Cmd 'Telescope git_branches', 'Checkout branch' },
		c = { Cmd 'Telescope colorscheme', 'Colorscheme' },
		f = {
			Cmd 'Telescope current_buffer_fuzzy_find theme=ivy previewer=false',
			'Buffer',
		},
		F = {
			function()
				require('telescope.builtin').live_grep {
					grep_open_files = true,
				}
			end,
			'Buffers',
		},
		r = { Cmd 'Telescope oldfiles', 'Open Recent File' },
		R = { Cmd 'Telescope registers', 'Registers' },
		k = { Cmd 'Telescope keymaps', 'Keymaps' },
		C = { Cmd 'Telescope commands', 'Commands' },
		-- p = { Cmd 'Telescope live_grep_args', 'Live Grep' },
		p = {
			function() require('plugins.telescope.pickers.rg').live_grepper {} end,
			'Live Grep',
		},
		w = {
			function()
				require('telescope-live-grep-args.shortcuts').grep_word_under_cursor {
					cwd = vim.fs.root(0, '.git'),
				}
			end,
			'Word',
		},
		l = { Cmd 'Telescope lsp_document_symbols', 'Symbol' },
		L = { ':Telescope lsp_workspace_symbols query=', 'Global Symbols' },
		s = { Cmd 'Spectre', 'Spectre' },
		y = { Cmd 'Telescope neoclip', 'Clipboard' },
	},

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

	-- open, Open, openers, Openers
	o = {
		name = 'Open',
		a = { Cmd 'AerialToggle! left', 'Aerial' },
		-- if you can't beat 'em
		c = { Cmd 'silent !code %', 'VSCode' },
		d = { Cmd 'Oil', 'Dir' },
		i = { Cmd 'OpenInitBuffer', 'Open init buffer' },
		l = { function() require('codethread.logger').select() end, 'open log file' },
		n = { Cmd 'LogOpen notifications', 'Notifcation History' },
		N = { function() Snacks.notifier.hide() end, 'Hide Notifications' },
	},

	t = {
		name = 'Terminal',
		n = { Cmd 'lua _NODE_TOGGLE()', 'Node' },
		t = { Cmd 'ToggleTerm direction=float', 'Float' },
		h = { Cmd 'ToggleTerm size=10 direction=horizontal', 'Horizontal' },
		v = { Cmd 'ToggleTerm size=80 direction=vertical', 'Vertical' },
		r = { Cmd 'TermExec Cmd="eslint_d restart"', 'Vertical' },
	},

	T = {
		name = 'Toggle',
		i = { Cmd "lua print'nothing setup'", 'Inlay Hints' },
		[' '] = { fns.toggle_listchars, 'Whitespace' },
		['>'] = { fns.toggle_indent_scope, 'IndentScope' },
	},

	u = {
		name = 'Utils',
		f = { "mbggVG=='b", 'format buffer' },
		s = { '1z=', 'autocorrect' },
		b = { function() require('codethread.box').box() end, 'Box' },
		r = {
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
	},

	-- clipboard, copy
	y = {
		name = 'Yank',
		l = { fns.copy_filepath_relative, 'path [gitlab]' },
		r = { Cmd 'let @*=@%', 'path [root]' }, -- TODO:
		y = { Cmd 'let @*=@%', 'path [relative]' }, -- TODO:
		s = { Cmd 'Telescope neoclip', 'Search' },
	},
}
