local status_ok, configs = pcall(require, 'nvim-treesitter.configs')
if not status_ok then
	print 'could not load treesitter'
	return
end

configs.setup {
	ensure_installed = {
		'bash',
		'c',
		'css',
		'dockerfile',
		'elixir',
		'gitignore',
		'go',
		'graphql',
		'html',
		'help', -- experimental
		'javascript',
		'json',
		'jsdoc',
		'json5',
		'jsonc',
		'lua',
		'make',
		'markdown',
		'markdown_inline',
		'regex',
		'rust',
		'svelte',
		'swift',
		'toml',
		'tsx',
		'typescript',
		'vim',
		'yaml',
	}, -- one of "all",(parsers with maintainers), or a list of languages

	-- sync_install = false, -- install languages synchronously (only applied to `ensure_installed`)
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
