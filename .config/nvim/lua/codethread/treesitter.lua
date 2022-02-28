local status_ok, configs = pcall(require, "nvim-treesitter.configs")
if not status_ok then
	print("could not load treesitter")
	return
end

configs.setup({
	ensure_installed = "maintained", -- one of "all", "maintained" (parsers with maintainers), or a list of languages
	sync_install = false, -- install languages synchronously (only applied to `ensure_installed`)
	ignore_install = { "" }, -- List of parsers to ignore installing

	autopairs = {
		enable = true,
	},

	highlight = {
		enable = true, -- false will disable the whole extension
		disable = { "" }, -- list of language that will be disabled
		additional_vim_regex_highlighting = false, -- use regex highlighting too, this is slow
	},
	indent = { enable = true, disable = { "yaml" } },

	context_commentstring = {
		enable = true,
		enable_autocmd = false,
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