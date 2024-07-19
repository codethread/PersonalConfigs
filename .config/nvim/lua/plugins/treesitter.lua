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
					multiline_threshold = 1,
					-- max_lines = 2, -- How many lines the window should span. Values <= 0 mean no limit.
					-- trim_scope = 'inner', -- Which context lines to discard if `max_lines` is exceeded. Choices: 'inner', 'outer'
				},
			},
			{ 'nushell/tree-sitter-nu' },
		},
		---@type TSConfig
		opts = {
			highlight = {
				enable = true,
				additional_vim_regex_highlighting = false,
			},
			indent = { enable = true },
			matchup = {
				enable = true,
			},
            -- stylua: ignore
            ensure_installed = vim.tbl_flatten {
                -- scripting
				{ 'awk','bash','jq','python' },
				-- lang
				{ 'c','rust','go','gomod','scala','haskell' },
				-- misc
				{ 'comment','todotxt','markdown','markdown_inline','query','regex' },
				--web
				{ 'css','scss','html','jsdoc','javascript','typescript','tsx','graphql', 'styled' },
				-- webish
				{ 'embedded_template','http','prisma','proto','svelte' },
				-- config
				{ 'dockerfile','json','json5','jsonc','make','toml','yaml' },
				-- git
				{ 'diff','git_rebase','gitattributes','gitcommit' },
				-- vim
				{ 'vim','vimdoc','lua','luadoc','luap' },
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
						-- ['ai'] = '@call.outer', -- i for 'invocation'
						-- ['ii'] = '@call.inner',
						['aa'] = '@parameter.outer',
						['ia'] = '@parameter.inner',

						['ab'] = '@conditional.outer', -- b for 'branch'
						['ib'] = '@conditional.inner',

						['ai'] = '@import.outer',
						['ii'] = '@import.inner',
						-- af = '@custom-field.outer',
						-- ['if'] = '@custom-field.inner',
					},
				},
				swap = {
					enable = true,
					swap_next = {
						['<leader>}'] = '@parameter.outer',
						-- ['<leader>}'] = '@conditional.inner',
					},
					swap_previous = {
						['<leader>{'] = '@parameter.outer',
					},
				},
				lsp_interop = {
					enable = true,
					border = 'none',
					floating_preview_opts = {},
					peek_definition_code = {
						['<leader>lp'] = '@function.outer', -- TODO: YUMMY
						['<leader>lP'] = '@class.outer', -- not sure how these differ?
					},
				},
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
				enable_close_on_slash = false, -- till https://github.com/aca/emmet-ls/issues/69 is resolved
			},
		},
	},
}
