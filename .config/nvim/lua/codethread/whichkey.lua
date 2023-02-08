-- :checkhealth which_key
local cmd = vim.ct.cmd
local require = require('codethread.utils').require
local which_key, ok = require 'which-key'
if not ok then return end

local setup = {
	plugins = {
		marks = true, -- shows a list of your marks on ' and `
		registers = true, -- shows your registers on " in NORMAL or <C-r> in INSERT mode
		spelling = {
			enabled = true, -- enabling this will show WhichKey when pressing z= to select spelling suggestions
			suggestions = 20, -- how many suggestions should be shown in the list?
		},
		-- the presets plugin, adds help for a bunch of default keybindings in Neovim
		-- No actual key bindings are created
		presets = {
			operators = false, -- adds help for operators like d, y, ... and registers them for motion / text object completion
			motions = true, -- adds help for motions
			text_objects = true, -- help for text objects triggered after entering an operator
			windows = true, -- default bindings on <c-w>
			nav = true, -- misc bindings to work with windows
			z = true, -- bindings for fold, spelling and others prefixed with z
			g = false, -- bindings for prefixed with g
		},
	},
	-- add operators that will trigger motion and text object completion
	-- to enable all native operators, set the preset / operators plugin above
	-- operators = { gc = "Comments" },
	key_labels = {
		-- override the label used to display some keys. It doesn't effect WK in any other way.
		-- For example:
		['<space>'] = '<SPC>',
		['<cr>'] = '<RET>',
		['<tab>'] = '<TAB>',
	},
	icons = {
		breadcrumb = '»', -- symbol used in the command line area that shows your active key combo
		separator = '➜', -- symbol used between a key and it's label
		group = '+', -- symbol prepended to a group
	},
	popup_mappings = {
		scroll_down = '<c-d>', -- binding to scroll down inside the popup
		scroll_up = '<c-u>', -- binding to scroll up inside the popup
	},
	window = {
		border = 'rounded', -- none, single, double, shadow
		position = 'bottom', -- bottom, top
		margin = { 1, 0, 1, 0 }, -- extra window margin [top, right, bottom, left]
		padding = { 2, 2, 2, 2 }, -- extra window padding [top, right, bottom, left]
		winblend = 0,
	},
	layout = {
		height = { min = 4, max = 25 }, -- min and max height of the columns
		width = { min = 20, max = 50 }, -- min and max width of the columns
		spacing = 3, -- spacing between columns
		align = 'left', -- align columns left, center or right
	},
	ignore_missing = true, -- enable this to hide mappings for which you didn't specify a label
	hidden = { '<silent>', cmd '', '<Cmd>', '', 'call', 'lua', '^:', '^ ' }, -- hide mapping boilerplate
	show_help = true, -- show help message on the command line when the popup is visible
	triggers = 'auto', -- automatically setup triggers
	-- triggers = {"<leader>"} -- or specify a list manually
	triggers_blacklist = {
		-- list of mode / prefixes that should never be hooked by WhichKey
		-- this is mostly relevant for key maps that start with a native binding
		-- most people should not need to change this
		i = { 'j', 'k' },
		v = { 'j', 'k' },
	},
}

local opts = {
	mode = 'n', -- NORMAL mode
	prefix = '<leader>',
	buffer = nil, -- Global mappings. Specify a buffer number for buffer local mappings
	silent = true, -- use `silent` when creating keymaps
	noremap = true, -- use `noremap` when creating keymaps
	nowait = true, -- use `nowait` when creating keymaps
}

-- " MAPS ON COMMANDS I DONT LIKE
-- " map <C-B>
-- " map <C-G>
-- " map <C-Q>
-- " map <C-Y>
-- map <C-F> :%s/
-- map <C-P> :Files<CR>
-- map \ :GrepFzf<CR>
local dap = require 'dap'

local mappings = {
	['<leader>'] = { cmd 'Telescope find_files shorten_path=false', 'Files' },
	[';'] = { cmd 'Telescope commands', 'M-x' },
	[':'] = { cmd 'Telescope command_history', 'M-x [hist]' },
	['/'] = { cmd 'Telescope search_history', '/ [hist]' },
	-- ["a"] = { cmd'Alpha', "Alpha" },
	['q'] = { cmd 'w | luafile %', 'Reload Luafile' },
	-- ["h"] = { cmd'nohlsearch', "No Highlight" },
	-- ["F"] = { cmd'Telescope live_grep theme=ivy', "Find Text" },
	-- ["P"] = { cmd'lua require('telescope').extensions.projects.projects()', "Projects" },

	b = {
		name = 'Buffers',
		b = { [[<C-^>]], 'Toggle' },
		l = {
			-- cmd'lua require('telescope.builtin').buffers(require('telescope.themes').get_dropdown{previewer = false})',
			cmd 'Telescope buffers',
			'list',
		},
		k = {
			cmd 'Bdelete',
			'kill',
		},
		s = { cmd 'w', 'Save' },
	},

	e = {
		name = 'Errors',
		l = {
			cmd 'Telescope diagnostics theme=ivy bufnr=0',
			'Document Diagnostics',
		},
		L = {
			cmd 'Telescope diagnostics',
			'Workspace Diagnostics',
		},
		h = {
			function() require('lsp_lines').toggle() end,
			'Toggle Diagnostics',
		},

		n = {
			cmd 'lua vim.diagnostic.goto_next()',
			'Next Diagnostic',
		},
		p = {
			cmd 'lua vim.diagnostic.goto_prev()',
			'Prev Diagnostic',
		},
		q = { cmd 'lua vim.diagnostic.setloclist()', 'Quickfix' },
	},

	d = {
		function()
			-- TODO: potentiall save all buffers first
			local dap = require 'dap'
			if dap.session() then
				require('codethread.lsp.dap-hydra').debug_running_hydra:activate()
			else
				require('codethread.lsp.dap-hydra').debug_hydra:activate()
			end
		end,
		'🐉 Debug',
	},

	f = {
		name = 'Fold?',
		f = {
			'za',
			'toggle',
		},
	},

	j = {
		name = 'Test',
		j = {
			function()
				local ft = vim.ct.ft()
				if ft == 'lua' then
					require('plenary.test_harness').test_directory(vim.fn.expand '%:p')
					-- also PlenaryTestFile
				elseif ft == 'javascript' then
					local jest = require 'jester'
					jest.run_last { path_to_jest = './node_modules/bin/jest' }
				else
					print('no setup for filetype: ' .. ft)
				end
			end,
			'file',
		},
	},

	g = {
		name = 'Git',
		R = { function() require('gitsigns').reset_buffer() end, 'Reset Buffer' },
		b = { cmd 'Telescope git_branches', 'Checkout branch' },
		c = { cmd 'Telescope git_commits', 'Checkout commit' },
		d = { cmd 'Gitsigns diffthis HEAD', 'Diff' },
		g = { function() require('neogit').open() end, 'Status' },
		j = { function() require('gitsigns').next_hunk() end, 'Next Hunk' },
		k = { function() require('gitsigns').prev_hunk() end, 'Prev Hunk' },
		l = { function() require('gitsigns').blame_line() end, 'Blame' },
		o = { cmd 'Telescope git_status', 'Open changed file' },
		p = { function() require('gitsigns').preview_hunk() end, 'Preview Hunk' },
		r = { function() require('gitsigns').reset_hunk() end, 'Reset Hunk' },
		s = { function() require('gitsigns').stage_hunk() end, 'Stage Hunk' },
		u = { function() require('gitsigns').undo_stage_hunk() end, 'Undo Stage Hunk' },
		v = { cmd 'silent !gh repo view --web', 'Ghub view' },
	},

	G = {
		name = 'Global',
		p = {
			cmd 'source ~/.config/nvim/lua/codethread/plugins.lua | PackerSync',
			'Packer Sync',
		},
		v = {
			cmd 'sourc ~/.config/nvim/init.lua',
			'Reload vimrc',
		},
	},

	h = {
		name = 'Help',
		h = { cmd 'Telescope help_tags', 'Help' },
		m = { cmd 'Telescope man_pages', 'Man' },
		v = { cmd 'Telescope vim_options', 'Settings' },
		t = { cmd 'Telescope builtin', 'Telescope' },
		c = { cmd 'Telescope highlights', 'Telescope' },
	},

	l = {
		name = 'LSP',
		-- a = { cmd'Telescope lsp_code_actions them=cursor', "Code Action" },
		a = {
			cmd 'lua vim.lsp.buf.code_action()',
			'Code Action',
		},
		i = { cmd 'LspInfo', 'Info' },
		I = { cmd 'LspInstallInfo', 'Installer Info' },
		l = { cmd 'lua vim.lsp.codelens.run()', 'CodeLens Action' },
		r = { cmd 'lua vim.lsp.buf.rename()', 'Rename' },
		s = { cmd 'Telescope lsp_document_symbols', 'Document Symbols' },
		S = {
			cmd 'Telescope lsp_dynamic_workspace_symbols',
			'Workspace Symbols',
		},
	},

	['m'] = {
		function() require('codethread.lsp.dap').mover_hydra:activate() end,
		'🐉 Mover',
	},

	w = {
		name = 'Window',
		N = { cmd 'tabnew', 'New Tab' },
		l = { function() require('telescope-tabs').list_tabs() end, 'List Tabs' }, -- TODO: put through telescope
		n = { cmd 'tabNext', 'Next Tab' }, -- TODO: put through telescope
		p = { cmd 'tabprevious', 'Previous Tab' }, -- TODO: put through telescope
		w = { cmd 'vsplit', 'Split' }, -- TODO: put through telescope
		s = { cmd 'SwapSplit', 'Swap' },
		e = {
			function() require('codethread.movement').tab_hydra:activate() end,
			'🐉 Tabs',
		},
	},

	s = {
		name = 'Search',
		b = { cmd 'Telescope git_branches', 'Checkout branch' },
		c = { cmd 'Telescope colorscheme', 'Colorscheme' },
		f = { cmd 'Telescope current_buffer_fuzzy_find theme=ivy previewer=false', 'Buffer' },
		h = { cmd 'Telescope help_tags', 'Find Help' },
		M = { cmd 'Telescope man_pages', 'Man Pages' },
		r = { cmd 'Telescope oldfiles', 'Open Recent File' },
		R = { cmd 'Telescope registers', 'Registers' },
		k = { cmd 'Telescope keymaps', 'Keymaps' },
		C = { cmd 'Telescope commands', 'Commands' },
		p = { cmd 'Telescope live_grep', 'Live Grep' },
		w = { cmd 'Telescope grep_string', 'Word' },
	},

	o = {
		name = 'Open',
		d = { cmd 'NvimTreeFindFile', 'File Tree' },
		i = { cmd 'OpenInitBuffer', 'Open init buffer' },
	},

	t = {
		name = 'Terminal',
		n = { cmd 'lua _NODE_TOGGLE()', 'Node' },
		t = { cmd 'ToggleTerm direction=float', 'Float' },
		h = { cmd 'ToggleTerm size=10 direction=horizontal', 'Horizontal' },
		v = { cmd 'ToggleTerm size=80 direction=vertical', 'Vertical' },
		r = { '<cmd>TermExec cmd="eslint_d restart"', 'Vertical' },
	},

	T = {
		name = 'Toggle',
		i = {
			cmd "lua print'nothing setup'",
			'Inlay Hints',
		},
	},

	u = {
		name = 'Utils',
		f = {
			"mbggVG=='b",
			'format buffer',
		},
	},
}

which_key.setup(setup)
which_key.register(mappings, opts)

which_key.register({
	d = {
		function()
			local selection = vim.ct.get_visual_selection()
			local ft = vim.ct.ft()
			if ft == 'lua' then
				-- obviously could do way more with treesitter
				local vars = vim.split(selection, ',', { trimempty = true })
				local printf = 'P({ '
				for _, v in pairs(vars) do
					printf = printf .. ' ' .. v .. ' = ' .. v .. ', '
				end
				printf = printf .. ' })'
				local row = vim.ct.current_pos()
				vim.api.nvim_buf_set_lines(0, row, row, false, { printf })
				vim.notify(printf)
			end
		end,
		'printf debug',
	},
}, {
	mode = 'v', -- NORMAL mode
	prefix = '<leader>',
	buffer = nil, -- Global mappings. Specify a buffer number for buffer local mappings
	silent = true, -- use `silent` when creating keymaps
	noremap = true, -- use `noremap` when creating keymaps
	nowait = true, -- use `nowait` when creating keymaps
})
