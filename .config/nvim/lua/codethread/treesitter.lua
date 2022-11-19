local status_ok, configs = pcall(require, 'nvim-treesitter.configs')
if not status_ok then
	print 'could not load treesitter'
	return
end

configs.setup {
	ensure_installed = 'all', -- parsers with maintainers
	auto_install = true,
	sync_install = false, -- install languages synchronously (only applied to `ensure_installed`)
	-- ignore_install = { "" }, -- List of parsers to ignore installing

	autopairs = {
		enable = true,
	},

	highlight = {
		enable = true, -- false will disable the whole extension
		disable = { '' }, -- list of language that will be disabled
		additional_vim_regex_highlighting = false, -- use regex highlighting too, this is slow
	},

	indent = {
		enable = true,
		-- disable = { "yaml" }
		disable = {},
	},

	context_commentstring = {
		enable = true,
	},

	incremental_selection = {
		enable = true,
		keymaps = {
			init_selection = 'gnn',
			node_incremental = '<M-j>',
			scope_incremental = '<M-h>',
			node_decremental = '<M-k>',
		},
	},

	playground = {
		enable = false,
	},

	textobjects = {
		select = {
			enable = true,
			-- Automatically jump forward to textobj, similar to targets.vim
			lookahead = false,
			keymaps = {
				-- You can use the capture groups defined in textobjects.scm
				['af'] = '@function.outer',
				['if'] = '@function.inner',
				['ac'] = '@class.outer',
				['ic'] = '@class.inner',
				['ai'] = '@call.outer', -- i for 'invocation'
				['ii'] = '@call.inner',
				['aa'] = '@parameter.outer',
				['ia'] = '@parameter.inner',

				['ab'] = '@conditional.outer', -- b for 'branch'
				['ib'] = '@conditional.inner',
			},
		},
		swap = { enable = true },
	},

	autotag = {
		enable = true,
	},
}

local ft_to_parser = require('nvim-treesitter.parsers').filetype_to_parsername
ft_to_parser.json = 'jsonc'

require('treesitter-context').setup {
	max_lines = 2, -- How many lines the window should span. Values <= 0 mean no limit.
	trim_scope = 'inner', -- Which context lines to discard if `max_lines` is exceeded. Choices: 'inner', 'outer'
}
