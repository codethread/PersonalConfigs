return {
	{
		'jose-elias-alvarez/null-ls.nvim',
		event = { 'BufReadPre', 'BufNewFile' },
		dependencies = { 'mason.nvim' },
		opts = function()
			local null_ls = require 'null-ls'
			-- https://github.com/jose-elias-alvarez/null-ls.nvim/tree/main/lua/null-ls/builtins/formatting
			local formatting = null_ls.builtins.formatting

			-- https://github.com/jose-elias-alvarez/null-ls.nvim/tree/main/lua/null-ls/builtins/diagnostics
			local diagnostics = null_ls.builtins.diagnostics
			local code_actions = null_ls.builtins.code_actions
			local augroup = vim.api.nvim_create_augroup('LspFormatting', {})

			return {
				root_dir = require('null-ls.utils').root_pattern(
					'.null-ls-root',
					'.neoconf.json',
					'Makefile',
					'.git'
				),
				-- on_attach = function(client, bufnr) end,
				sources = {
					-- lua
					formatting.stylua,
					diagnostics.luacheck,
					-- rust
					formatting.rustfmt.with {
						extra_args = function(params)
							local Path = require 'plenary.path'
							local cargo_toml = Path:new(params.root .. '/' .. 'Cargo.toml')

							if cargo_toml:exists() and cargo_toml:is_file() then
								for _, line in ipairs(cargo_toml:readlines()) do
									local edition = line:match [[^edition%s*=%s*%"(%d+)%"]]
									if edition then return { '--edition=' .. edition } end
								end
							end
							-- default edition when we don't find `Cargo.toml` or the `edition` in it.
							return { '--edition=2021' }
						end,
					},

					-- js/ts
					formatting.prettierd.with {
						filetypes = {
							'javascript',
							'javascriptreact',
							'typescript',
							'typescriptreact',
							'vue',
							'css',
							'scss',
							'less',
							'html',
							'json',
							'jsonc',
							'yaml',
							'markdown',
							'graphql',
							'handlebars',
							'svelte',
						},
					},
					-- diagnostics.stylelint.with {
					-- 	filetypes = {
					-- 		'scss',
					-- 		'less',
					-- 		'css',
					-- 		'sass',
					-- 		'javascript',
					-- 		'javascriptreact',
					-- 		'typescript',
					-- 		'typescriptreact',
					-- 	},
					-- 	args = {
					-- 		'--formatter',
					-- 		'json',
					-- 		'--stdin-filename',
					-- 		'$FILENAME',
					-- 		'--config=packages/irati/web/.stylelintrc',
					-- 	},
					-- },

					-- shell
					diagnostics.shellcheck,
					code_actions.shellcheck,

					-- using eslint lsp instead
					-- diagnostics.eslint_d.with({
					--   diagnostics_format = '[eslint] #{m}\n(#{c})'
					-- }),
					-- code_actions.eslint_d,

					-- c/c++
					formatting.clang_format,

					-- go
					formatting.gofmt,

					-- lisp
					formatting.joker.with {
						filetypes = { 'clojure' },
					},
				},
			}
		end,
	},
}
