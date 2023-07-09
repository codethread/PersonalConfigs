return {
	{
		'nvim-treesitter/nvim-treesitter',
		version = false, -- last release is way too old and doesn't work on Windows
		event = { 'BufReadPost', 'BufNewFile' },
		build = ':TSUpdate',
		dependencies = {
			{
				'nvim-treesitter/nvim-treesitter-context',
				opts = {
					-- max_lines = 2, -- How many lines the window should span. Values <= 0 mean no limit.
					-- trim_scope = 'inner', -- Which context lines to discard if `max_lines` is exceeded. Choices: 'inner', 'outer'
				},
			},
		},
		keys = {
			-- { '<c-space>', desc = 'Increment selection' },
			-- { '<bs>', desc = 'Decrement selection', mode = 'x' },
		},
		---@type TSConfig
		opts = {
			highlight = {
				enable = true,
				additional_vim_regex_highlighting = false,
			},
			indent = { enable = true },
            -- stylua: ignore
            ensure_installed = vim.tbl_flatten {
                -- scripting
                { 'awk',     'bash',    'jq',       'python' },
                -- lang
                { 'c',       'rust',    'go',       'gomod',           'scala', 'haskell', },
                -- misc
                { 'comment', 'todotxt', 'markdown', 'markdown_inline', 'query', 'regex', },
                --web
                { 'css', 'scss', 'html', 'jsdoc', 'javascript', 'typescript',
                    'tsx',
                    'graphql', },
                -- webish
                { 'embedded_template', 'http',       'prisma',        'proto',    'svelte' },
                -- config
                { 'dockerfile',        'json',       'json5',         'jsonc',    'make',  'toml', 'yaml', },
                -- git
                { 'diff',              'git_rebase', 'gitattributes', 'gitcommit' },
                -- vim
                { 'vim',               'vimdoc',     'lua',           'luadoc',   'luap', },
            },
		},
		---@param opts TSConfig
		config = function(_, opts)
			if type(opts.ensure_installed) == 'table' then
				---@type table<string, boolean>
				local added = {}
				opts.ensure_installed = vim.tbl_filter(function(lang)
					if added[lang] then return false end
					added[lang] = true
					return true
				end, opts.ensure_installed)
			end
			require('nvim-treesitter.configs').setup(opts)
			vim.treesitter.language.register('jsonc', 'json')
			vim.treesitter.language.register('dts', 'keymap')
		end,
	},
	{
		'nvim-treesitter/nvim-treesitter',
		dependencies = 'nvim-treesitter/nvim-treesitter-textobjects',
		opts = {
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

						-- af = '@custom-field.outer',
						-- ['if'] = '@custom-field.inner',
					},
				},
				swap = { enable = true },
			},
		},
	},
	{
		'nvim-treesitter/playground',
		cmd = 'TSPlaygroundToggle',
		dependencies = 'nvim-treesitter/nvim-treesitter',
	},
	{
		'nvim-treesitter/nvim-treesitter',
		lazy = true,
		dependencies = 'windwp/nvim-ts-autotag', -- close <div tags, and ciw
		---@type TSConfig
		opts = {
			autotag = {
				enable = true,
			},
		},
	},
}
