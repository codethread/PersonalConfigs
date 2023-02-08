local require = require('codethread.utils').require
local configs, ok = require 'nvim-treesitter.configs'
if not ok then return end

configs.setup {
	ensure_installed = {
		'awk',
		'bash',
		'c',
		'comment',
		'css',
		'diff',
		'dockerfile',
		'embedded_template',
		'git_rebase',
		'gitattributes',
		'gitcommit',
		'go',
		'gomod',
		'graphql',
		'haskell',
		'help',
		'html',
		'http',
		'javascript',
		'jq',
		'jsdoc',
		'json',
		'json5',
		'jsonc',
		'lua',
		'make',
		'markdown',
		'markdown_inline',
		'prisma',
		'proto',
		'query',
		'regex',
		'rust',
		'scala',
		'scss',
		'svelte',
		'todotxt',
		'toml',
		'tsx',
		'typescript',
		'vim',
		'yaml',
		-- 'agda',
		-- 'arduino',
		-- 'astro',
		-- 'beancount',
		-- 'bibtex',
		-- 'blueprint',
		-- 'c_sharp',
		-- 'clojure',
		-- 'cmake',
		-- 'commonlisp',
		-- 'cooklang',
		-- 'cpp',
		-- 'cuda',
		-- 'dart',
		-- 'dot',
		-- 'ebnf',
		-- 'eex',
		-- 'elixir',
		-- 'elm',
		-- 'elvish',
		-- 'erlang',
		-- 'fennel',
		-- 'fish',
		-- 'foam',
		-- 'fortran',
		-- 'fusion',
		-- 'gdscript',
		-- 'gleam',
		-- 'glimmer',
		-- 'glsl',
		-- 'gowork',
		-- 'hack',
		-- 'hcl',
		-- 'heex',
		-- 'hjson',
		-- 'hlsl',
		-- 'hocon',
		-- 'java',
		-- 'jsonnet',
		-- 'julia',
		-- 'kotlin',
		-- 'lalrpop',
		-- 'latex',
		-- 'ledger',
		-- 'llvm',
		-- 'm68k',
		-- 'menhir',
		-- 'mermaid',
		-- 'meson',
		-- 'nickel',
		-- 'ninja',
		-- 'nix',
		-- 'norg',
		-- 'ocaml',
		-- 'ocaml_interface',
		-- 'org',
		-- 'pascal',
		-- 'perl',
		-- 'php',
		-- 'phpdoc',
		-- 'pioasm',
		-- 'pug',
		-- 'python',
		-- 'ql',
		-- 'qmljs',
		-- 'r',
		-- 'racket',
		-- 'rasi',
		-- 'rego',
		-- 'rnoweb',
		-- 'rst',
		-- 'ruby',
		-- 'scheme',
		-- 'slint',
		-- 'solidity',
		-- 'sparql',
		-- 'supercollider',
		-- 'surface',
		-- 'sxhkdrc',
		-- 'tiger',
		-- 'tlaplus',
		-- 'turtle',
		-- 'twig',
		-- 'v',
		-- 'vala',
		-- 'verilog',
		-- 'vhs',
		-- 'vue',
		-- 'wgsl',
		-- 'yang',
		-- 'zig',
	},

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
			node_incremental = '<C-j>',
			scope_incremental = '<C-h>',
			node_decremental = '<C-k>',
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

				-- af = '@custom-field.outer',
				-- ['if'] = '@custom-field.inner',
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
