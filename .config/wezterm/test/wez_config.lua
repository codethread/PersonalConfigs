return {
	Lua = {
		completion = {
			callSnippet = "Replace",
		},
		diagnostics = {
			globals = { "vim", "K", "U", "Cmd", "Lua", "Term", "async", "await", "utf8" },
		},
		runtime = {
			path = { "?.lua", "?/init.lua" },
			pathStrict = true,
			version = "5.4",
		},
		workspace = {
			checkThirdParty = false,
			ignoreDir = { "/lua" },
			library = {
				-- '/opt/homebrew/Cellar/neovim/HEAD-bf5c134_1/share/nvim/runtime/lua',
				-- '/opt/homebrew/share/lua/5.4',
				-- '/Users/codethread/PersonalConfigs/lua',
				"/Users/codethread/.local/share/nvim/lazy/wezterm-types",
				-- '/Users/codethread/PersonalConfigs/.config/wezterm',
			},
		},
	},
}
