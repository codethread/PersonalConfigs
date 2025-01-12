return {
	{
		'j-hui/fidget.nvim',
		lazy = true,
		-- tag = 'legacy',
		opts = {
			progress = {
				ignore = {},
				display = {
					done_icon = '✓',
				},
			},
		},
	},

	{
		'b0o/SchemaStore.nvim',
		lazy = true,
		version = false, -- last release is way too old
	},

	{
		-- when looking for lsp capabilities for supports_method use spec (example is a pinned version, use latest):
		-- https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#languageFeatures
		'neovim/nvim-lspconfig',
		event = { 'BufReadPre', 'BufNewFile' },
		dependencies = {
			'j-hui/fidget.nvim',
			'williamboman/mason.nvim',
			'williamboman/mason-lspconfig.nvim',
			'hrsh7th/nvim-cmp',
			'hrsh7th/cmp-nvim-lsp',
			{ 'folke/neoconf.nvim', cmd = 'Neoconf', config = true },

			{
				'folke/lazydev.nvim',
				dependencies = {
					-- { 'gonstoll/wezterm-types', lazy = true }, -- update of below
					-- { 'justinsgithub/wezterm-types', lazy = true },
					{ 'Bilal2453/luvit-meta', lazy = true },
				},
				ft = 'lua', -- only load on lua files
				opts = {
					---@module "lazydev"
					---@type lazydev.Library.spec[]
					library = {
						{ path = 'luvit-meta/library', words = { 'vim%.uv' } },
						{ path = 'wezterm-types', mods = { 'wezterm' } },
					},
					enabled = function(root_dir)
						-- disable when a .luarc.json file is found
						if vim.uv.fs_stat(root_dir .. '/.luarc.json') then return false end
						return true
					end,
				},
			},
		},
		---@diagnostic disable: missing-fields
		---@class PluginLspOpts
		opts = {
			-- options for vim.diagnostic.config()
			diagnostics = {},

			-- LSP Server Settings
			---@type lspconfig.options
			servers = {
				jsonls = {
					on_new_config = function(new_config)
						new_config.settings.json.schemas = new_config.settings.json.schemas or {}
						vim.list_extend(new_config.settings.json.schemas, require('schemastore').json.schemas())
					end,
					settings = {
						json = {
							format = {
								enable = true,
							},
							validate = { enable = true },
						},
					},
				},
				nushell = {
					cmd = {
						'nu',
						'--lsp',
						'--env-config=' .. vim.fn.expand '~/PersonalConfigs/.config/nushell/env.nu',
						'--config=' .. vim.fn.expand '~/PersonalConfigs/.config/nushell/config.nu',
						'--include-path=' .. vim.fn.expand '~/PersonalConfigs/.config/nushell/scripts',
					},
				},
				wgsl_analyzer = {
					settings = {},
				},
				-- clangd = {
				-- 	filetypes = { 'c', 'cpp', 'objc', 'objcpp', 'cuda' },
				-- },
				eslint = {
					settings = {
						run = 'onSave',
					},
				},
				lua_ls = {
					settings = {
						Lua = {
							hint = { enable = true },
							workspace = {
								checkThirdParty = false,
							},
							completion = {
								callSnippet = 'Replace',
							},
							diagnostics = {
								globals = {
									'vim',
									'K',
									'U',
									'Cmd',
									'Lua',
									'Term',
									'async',
									'await',
									'utf8',
								},
							},
						},
					},
				},
			},
			setup = {},
		},
		config = function(_, opts)
			U.lsp_attach('*', function(client, buf)
				-- NOTE: uncomment to see
				-- vim.print(client.server_capabilities)

				U.keys(buf, {
					{ 'gD', function() vim.lsp.buf.declaration() end, 'declaration' },
					{ 'gd', function() vim.lsp.buf.definition { reuse_win = true } end, 'definition' },
					{
						'K',
						function()
							local winid = require('ufo').peekFoldedLinesUnderCursor()
							if not winid then vim.lsp.buf.hover() end
						end,
						'hover',
					},
					{ 'gi', function() vim.lsp.buf.implementation() end, 'implementation' },
					{ 'gh', function() vim.lsp.buf.signature_help() end, 'signature_help' },
					{ 'gr', function() vim.lsp.buf.references() end, 'references' },
				}, { prefix = '', unique = false })
			end)

			U.lsp_attach('*', function(client, bufnr)
				-- highlight on hover
				if client.server_capabilities.documentHighlightProvider then
					vim.api.nvim_create_augroup('lsp_document_highlight', { clear = true })
					vim.api.nvim_clear_autocmds { buffer = bufnr, group = 'lsp_document_highlight' }
					vim.api.nvim_create_autocmd('CursorHold', {
						callback = vim.lsp.buf.document_highlight,
						buffer = bufnr,
						group = 'lsp_document_highlight',
						desc = 'Document Highlight',
					})
					vim.api.nvim_create_autocmd('CursorMoved', {
						callback = vim.lsp.buf.clear_references,
						buffer = bufnr,
						group = 'lsp_document_highlight',
						desc = 'Clear All the References',
					})
				end
			end)

			vim.diagnostic.config(vim.deepcopy(opts.diagnostics))

			local servers = opts.servers

			local function setup(server)
				local server_opts = vim.tbl_deep_extend('force', {
					capabilities = vim.deepcopy(require 'plugins.lsp.capabilities'(opts.capabilities)),
				}, servers[server] or {})

				-- disable snippets in autocomplete
				-- TODO this is needed as true for JSONls, but if annoying will conditionally disable
				server_opts.capabilities.textDocument.completion.completionItem.snippetSupport = true

				if opts.setup[server] then
					if opts.setup[server](server, server_opts) then return end
				elseif opts.setup['*'] then
					if opts.setup['*'](server, server_opts) then return end
				end
				require('lspconfig')[server].setup(server_opts)
			end

			-- get all the servers that are available thourgh mason-lspconfig
			local mlsp = require 'mason-lspconfig'
			local all_mslp_servers =
				vim.tbl_keys(require('mason-lspconfig.mappings.server').lspconfig_to_package)

			local ensure_installed = {} ---@type string[]
			for server, server_opts in pairs(servers) do
				if server_opts then
					server_opts = server_opts == true and {} or server_opts
					-- run manual setup if mason=false or if this is a server that cannot be installed with mason-lspconfig
					if server_opts.mason == false or not vim.tbl_contains(all_mslp_servers, server) then
						setup(server)
					else
						ensure_installed[#ensure_installed + 1] = server
					end
				end
			end

			mlsp.setup { ensure_installed = ensure_installed }
			mlsp.setup_handlers { setup }
		end,
	},
}
