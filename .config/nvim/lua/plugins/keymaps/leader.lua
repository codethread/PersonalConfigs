local diagnostics_active = true
local function toggle_diagnostic()
	diagnostics_active = not diagnostics_active
	if diagnostics_active then
		vim.diagnostic.show()
	else
		vim.diagnostic.hide()
	end
end

return {
	['<leader>'] = { Cmd 'Telescope find_files', 'Files' },
	[']'] = { name = '+next' },
	['['] = { name = '+prev' },
	-- ['<leader>v'] = { name = '+vim' },
	[';'] = { Cmd 'Telescope commands', 'M-x' },
	[':'] = { Cmd 'Telescope command_history', 'M-x [hist]' },
	['/'] = { Cmd 'Telescope search_history', '/ [hist]' },
	['q'] = { Cmd 'w | luafile %', 'Reload Luafile' },

	a = { Cmd 'AerialToggle! left', 'Aerial' },

	b = {
		name = '...',
	},

	e = {
		name = 'Errors',
		h = { toggle_diagnostic, 'Toggle Diagnostics' },
		l = { Cmd 'Telescope diagnostics theme=ivy bufnr=0', 'Document Diagnostics' },
		L = { Cmd 'Telescope diagnostics', 'Workspace Diagnostics' },
		n = { Cmd 'lua vim.diagnostic.goto_next()', 'Next Diagnostic' },
		p = { Cmd 'lua vim.diagnostic.goto_prev()', 'Prev Diagnostic' },
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
		k = { Cmd 'Bdelete', 'kill' },
		s = {
			function()
				local ft = U.ft()
				if ft == 'oil' then
					require('oil').save(nil, function(err)
						if err then
							vim.notify(err, vim.log.levels.ERROR, { title = 'Dotty' })
						else
							require('codethread.dotty').dotty_link()
						end
					end)
				else
					vim.cmd.w()
				end
			end,
			'Save',
		},
		u = { function() require('codethread.pickers').unsaved() end, 'Unsaved' },
	},

	j = {
		name = 'Test',
		j = {
			function()
				local ft = U.ft()
				if ft == 'lua' then
					vim.cmd 'w'
					-- require('plenary.test_harness').test_directory(vim.fn.expand '%:p')
					require('plenary.test_harness').test_directory(
						vim.fn.expand '%:p',
						-- if writing lua tests, I'll follow the same setup as https://github.com/m00qek/plugin-template.nvim
						{ minimal_init = 'test/spec.vim' }
					)
					-- also PlenaryTestFile
				elseif ft == 'javascript' then
					local jest = require 'jester'
					jest.run_last { path_to_jest = './node_modules/bin/jest' }
				elseif ft == 'go' then
					-- vim.Cmd.GoTestFunc()
					vim.cmd.GoTestSubCase()
				else
					print('no setup for filetype: ' .. ft)
				end
			end,
			'file',
		},
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
		j = { function() require('gitsigns').next_hunk() end, 'Next Hunk' },
		k = { function() require('gitsigns').prev_hunk() end, 'Prev Hunk' },
		h = { Cmd 'DiffviewFileHistory % --no-merges', 'File History' },
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
		m = { Cmd 'Telescope man_pages', 'Man' },
		v = { Cmd 'Telescope vim_options', 'Settings' },
		t = { Cmd 'Telescope builtin', 'Telescope' },
		c = { Cmd 'Telescope highlights', 'Telescope' },
	},

	l = {
		name = 'LSP',
		-- a = { Cmd'Telescope lsp_code_actions them=cursor', "Code Action" },
		a = {
			Cmd 'lua vim.lsp.buf.code_action()',
			'Code Action',
		},
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

	s = {
		name = 'Search',
		b = { Cmd 'Telescope git_branches', 'Checkout branch' },
		c = { Cmd 'Telescope colorscheme', 'Colorscheme' },
		f = {
			Cmd 'Telescope current_buffer_fuzzy_find theme=ivy previewer=false',
			'Buffer',
		},
		h = { Cmd 'Telescope help_tags', 'Find Help' },
		M = { Cmd 'Telescope man_pages', 'Man Pages' },
		r = { Cmd 'Telescope oldfiles', 'Open Recent File' },
		R = { Cmd 'Telescope registers', 'Registers' },
		k = { Cmd 'Telescope keymaps', 'Keymaps' },
		C = { Cmd 'Telescope commands', 'Commands' },
		p = { Cmd 'Telescope live_grep_args', 'Live Grep' },
		w = {
			function() require('telescope-live-grep-args.shortcuts').grep_word_under_cursor() end,
			'Word',
		},
		l = { Cmd 'Telescope lsp_document_symbols', 'Symbol' },
		L = { ':Telescope lsp_workspace_symbols query=', 'Global Symbols' },
		s = { Cmd 'Spectre', 'Spectre' },
		y = { Cmd 'Telescope neoclip', 'Clipboard' },
	},

	n = {
		name = 'notes',
		a = { Cmd 'ObsidianNew', 'New' },
		i = { Cmd 'ObsidianTemplate', 'Template' },
		n = { Cmd 'ObsidianQuickSwitch', 'Find' },
		o = { Cmd 'ObsidianOpen', 'Open' },
		b = { Cmd 'ObsidianBacklinks', 'Backlinks' },
		s = { Cmd 'ObsidianSearch', 'Search' },
		t = { Cmd 'ObsidianToday', 'Today' },
		['1'] = {
			Cmd 'e ~/Library/Mobile Documents/iCloud~md~obsidian/Documents/Test/notes/project/neovim/nvim ideas.md',
			'nvim ideas.md',
		},
	},

	-- open, Open, openers, Openers
	o = {
		name = 'Open',
		i = { Cmd 'OpenInitBuffer', 'Open init buffer' },
		-- if you can't beat 'em
		c = { Cmd 'silent !code %', 'VSCode' },
		d = { Cmd 'Oil', 'Dir' },
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
	},

	u = {
		name = 'Utils',
		f = { "mbggVG=='b", 'format buffer' },
		y = { Cmd 'let @*=@%', 'save filename' },
	},
}
