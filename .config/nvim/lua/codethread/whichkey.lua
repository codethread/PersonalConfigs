-- :checkhealth which_key
local status_ok, which_key = pcall(require, 'which-key')

if not status_ok then
	print 'could not load which key'
	return
end

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
			g = true, -- bindings for prefixed with g
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
	hidden = { '<silent>', '<cmd>', '<Cmd>', '<CR>', 'call', 'lua', '^:', '^ ' }, -- hide mapping boilerplate
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

local mappings = {
	['<leader>'] = { '<cmd>Telescope find_files shorten_path=false<cr>', 'Files' },
	[';'] = { '<cmd>Telescope commands<cr>', 'M-x' },
	[':'] = { '<cmd>Telescope command_history<cr>', 'M-x [hist]' },
	['/'] = { '<cmd>Telescope search_history<cr>', '/ [hist]' },
	-- ["a"] = { "<cmd>Alpha<cr>", "Alpha" },
	['q'] = { '<cmd>w | luafile %<CR>', 'Reload Luafile' },
	-- ["h"] = { "<cmd>nohlsearch<CR>", "No Highlight" },
	-- ["F"] = { "<cmd>Telescope live_grep theme=ivy<cr>", "Find Text" },
	-- ["P"] = { "<cmd>lua require('telescope').extensions.projects.projects()<cr>", "Projects" },

	b = {
		name = 'Buffers',
		b = { [[<C-^>]], 'Toggle' },
		l = {
			-- "<cmd>lua require('telescope.builtin').buffers(require('telescope.themes').get_dropdown{previewer = false})<cr>",
			'<cmd>Telescope buffers<cr>',
			'list',
		},
		k = {
			'<cmd>Bdelete<cr>',
			'kill',
		},
		s = { '<cmd>w<CR>', 'Save' },
	},

	e = {
		name = 'Errors',
		l = {
			'<cmd>Telescope diagnostics theme=ivy bufnr=0<cr>',
			'Document Diagnostics',
		},
		L = {
			'<cmd>Telescope diagnostics<cr>',
			'Workspace Diagnostics',
		},
		h = {
			function() require('lsp_lines').toggle() end,
			'Toggle Diagnostics',
		},

		n = {
			'<cmd>lua vim.diagnostic.goto_next()<CR>',
			'Next Diagnostic',
		},
		p = {
			'<cmd>lua vim.diagnostic.goto_prev()<cr>',
			'Prev Diagnostic',
		},
		q = { '<cmd>lua vim.diagnostic.setloclist()<cr>', 'Quickfix' },
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
				local jest = require 'jester'
				jest.run_last { path_to_jest = './node_modules/bin/jest' }
			end,
			'file',
		},
	},

	g = {
		name = 'Git',
		R = { "<cmd>lua require 'gitsigns'.reset_buffer()<cr>", 'Reset Buffer' },
		b = { '<cmd>Telescope git_branches<cr>', 'Checkout branch' },
		c = { '<cmd>Telescope git_commits<cr>', 'Checkout commit' },
		d = { '<cmd>Gitsigns diffthis HEAD<cr>', 'Diff' },
		g = { "<cmd>lua require 'neogit'.open()<CR>", 'Status' },
		j = { "<cmd>lua require 'gitsigns'.next_hunk()<cr>", 'Next Hunk' },
		k = { "<cmd>lua require 'gitsigns'.prev_hunk()<cr>", 'Prev Hunk' },
		l = { "<cmd>lua require 'gitsigns'.blame_line()<cr>", 'Blame' },
		o = { '<cmd>Telescope git_status<cr>', 'Open changed file' },
		p = { "<cmd>lua require 'gitsigns'.preview_hunk()<cr>", 'Preview Hunk' },
		r = { "<cmd>lua require 'gitsigns'.reset_hunk()<cr>", 'Reset Hunk' },
		s = { "<cmd>lua require 'gitsigns'.stage_hunk()<cr>", 'Stage Hunk' },
		u = { "<cmd>lua require 'gitsigns'.undo_stage_hunk()<cr>", 'Undo Stage Hunk' },
		v = { '<cmd>silent !gh repo view --web<cr>', 'Ghub view' },
	},

	G = {
		name = 'Global',
		p = {
			'<cmd>source ~/.config/nvim/lua/codethread/plugins.lua | PackerSync<cr>',
			'Packer Sync',
		},
		v = {
			'<cmd>sourc ~/.config/nvim/init.lua<cr>',
			'Reload vimrc',
		},
	},

	h = {
		name = 'Help',
		h = { '<cmd>Telescope help_tags<cr>', 'Help' },
		m = { '<cmd>Telescope man_pages<cr>', 'Man' },
		v = { '<cmd>Telescope vim_options<cr>', 'Settings' },
		t = { '<cmd>Telescope builtin<cr>', 'Telescope' },
		c = { '<cmd>Telescope highlights<cr>', 'Telescope' },
	},

	l = {
		name = 'LSP',
		-- a = { "<cmd>Telescope lsp_code_actions them=cursor<cr>", "Code Action" },
		a = {
			'<cmd>lua vim.lsp.buf.code_action()<cr>',
			'Code Action',
		},
		i = { '<cmd>LspInfo<cr>', 'Info' },
		I = { '<cmd>LspInstallInfo<cr>', 'Installer Info' },
		l = { '<cmd>lua vim.lsp.codelens.run()<cr>', 'CodeLens Action' },
		r = { '<cmd>lua vim.lsp.buf.rename()<cr>', 'Rename' },
		s = { '<cmd>Telescope lsp_document_symbols<cr>', 'Document Symbols' },
		S = {
			'<cmd>Telescope lsp_dynamic_workspace_symbols<cr>',
			'Workspace Symbols',
		},
	},

	['m'] = {
		function() require('codethread.movement').mover_hydra:activate() end,
		'🐉 Mover',
	},

	w = {
		name = 'Window',
		N = { '<cmd>tabnew<cr>', 'New Tab' },
		l = { function() require('telescope-tabs').list_tabs() end, 'List Tabs' }, -- TODO: put through telescope
		n = { '<cmd>tabNext<cr>', 'Next Tab' }, -- TODO: put through telescope
		p = { '<cmd>tabprevious<cr>', 'Previous Tab' }, -- TODO: put through telescope
		w = { '<cmd>vsplit<cr>', 'Split' }, -- TODO: put through telescope
		s = { '<cmd>SwapSplit<cr>', 'Swap' },
		e = {
			function() require('codethread.movement').tab_hydra:activate() end,
			'🐉 Tabs',
		},
	},

	s = {
		name = 'Search',
		b = { '<cmd>Telescope git_branches<cr>', 'Checkout branch' },
		c = { '<cmd>Telescope colorscheme<cr>', 'Colorscheme' },
		f = { '<cmd>Telescope current_buffer_fuzzy_find theme=ivy previewer=false<cr>', 'Buffer' },
		h = { '<cmd>Telescope help_tags<cr>', 'Find Help' },
		M = { '<cmd>Telescope man_pages<cr>', 'Man Pages' },
		r = { '<cmd>Telescope oldfiles<cr>', 'Open Recent File' },
		R = { '<cmd>Telescope registers<cr>', 'Registers' },
		k = { '<cmd>Telescope keymaps<cr>', 'Keymaps' },
		C = { '<cmd>Telescope commands<cr>', 'Commands' },
		p = { '<cmd>Telescope live_grep<cr>', 'Live Grep' },
		w = { '<cmd>Telescope grep_string<cr>', 'Word' },
	},

	o = {
		name = 'Open',
		d = { '<cmd>NvimTreeFindFile<cr>', 'File Tree' },
	},

	t = {
		name = 'Terminal',
		n = { '<cmd>lua _NODE_TOGGLE()<cr>', 'Node' },
		t = { '<cmd>ToggleTerm direction=float<cr>', 'Float' },
		h = { '<cmd>ToggleTerm size=10 direction=horizontal<cr>', 'Horizontal' },
		v = { '<cmd>ToggleTerm size=80 direction=vertical<cr>', 'Vertical' },
		r = { '<cmd>TermExec cmd="eslint_d restart"', 'Vertical' },
	},

	T = {
		name = 'Toggle',
		i = {
			"<cmd>lua print'nothing setup'<cr>",
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
