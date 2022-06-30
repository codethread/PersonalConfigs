local status_ok, configs = pcall(require, "nvim-treesitter.configs")
if not status_ok then
	print("could not load treesitter")
	return
end

configs.setup({
	ensure_installed = {
		"bash",
		"c",
		"css",
		"dockerfile",
		"elixir",
		"go",
		"graphql",
		"html",
		"javascript",
		"json",
		"jsdoc",
		"json5",
		"jsonc",
		"lua",
		"make",
		"markdown",
		"regex",
		"rust",
		"svelte",
		"swift",
		"toml",
		"tsx",
		"typescript",
		"vim",
		"yaml",
	}, -- one of "all",(parsers with maintainers), or a list of languages

	-- sync_install = false, -- install languages synchronously (only applied to `ensure_installed`)
	-- ignore_install = { "" }, -- List of parsers to ignore installing

	autopairs = {
		enable = true,
	},

	highlight = {
		enable = true, -- false will disable the whole extension
		disable = { "" }, -- list of language that will be disabled
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

	playground = {
		enable = false,
	},

	textobjects = {
		select = {
			enable = true,

			-- Automatically jump forward to textobj, similar to targets.vim
			lookahead = true,

			keymaps = {
				-- You can use the capture groups defined in textobjects.scm
				["af"] = "@function.outer",
				["if"] = "@function.inner",
				["ac"] = "@class.outer",
				["ic"] = "@class.inner",
				["aa"] = "@parameter.outer",
				["ia"] = "@parameter.inner",
			},
		},
	},

	autotag = {
		enable = true,
	},
})

local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
parser_config.jsonc.used_by = "json"
