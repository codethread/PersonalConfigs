return {
	{
		'j-hui/fidget.nvim',
		lazy = true,
		-- tag = 'legacy',
		opts = { progress = { ignore = {}, display = { done_icon = '✓' } } },
	},

	{ 'b0o/SchemaStore.nvim', lazy = true },

	{
		'folke/lazydev.nvim',
		dependencies = {
			-- { 'gonstoll/wezterm-types', lazy = true }, -- update of below
			-- { 'justinsgithub/wezterm-types', lazy = true },
			{ 'Bilal2453/luvit-meta', lazy = true },
			'neovim/nvim-lspconfig',
		},
		ft = 'lua', -- only load on lua files
		opts = {
			---@module 'lazydev'
			---@type lazydev.Library.spec[]
			library = {
				{ path = 'luvit-meta/library', words = { 'vim%.uv' } },
				{ path = 'wezterm-types', mods = { 'wezterm' } },
				'snacks.nvim',
				'lazy.nvim',
			},
			enabled = function(root_dir)
				-- disable when a .luarc.json file is found
				if vim.uv.fs_stat(root_dir .. '/.luarc.json') then return false end
				return true
			end,
		},
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
			'hrsh7th/cmp-nvim-lsp',
			{ 'folke/neoconf.nvim', cmd = 'Neoconf', opts = {} }, -- adds lspconfig type
		},
		---@class PluginLspOpts
		opts = function()
			local nvim_lsp = require 'lspconfig'
			local root = nvim_lsp.util.root_pattern
			return {
				-- LSP Server Settings
				---@diagnostic disable: missing-fields
				---@type lspconfig.options
				servers = {
					jsonls = {
						on_new_config = function(new_config)
							new_config.settings.json.schemas = new_config.settings.json.schemas or {}
							vim.list_extend(
								new_config.settings.json.schemas,
								require('schemastore').json.schemas()
							)
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
					-- clangd = {
					-- 	filetypes = { 'c', 'cpp', 'objc', 'objcpp', 'cuda' },
					-- },
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
					tailwindcss = {
						root_dir = root 'tailwind.config.js',
						settings = {
							-- TODO: turn this off based on project, probably needs to look for the tailwind config
							tailwindCSS = {
								enable = false,
								experimental = {
									classRegex = {
										{
											'tv\\((([^()]*|\\([^()]*\\))*)\\)',
											'["\'`]([^"\'`]*).*?["\'`]',
										},
									},
								},
							},
						},
					},
					eslint = {
						settings = {
							run = 'onSave',
						},
					},
					denols = {
						root_dir = root('deno.json', 'deno.jsonc'),
						settings = {
							deno = {
								enable = false,
							},
						},
					},
					vtsls = {
						root_dir = root 'tsconfig.json',
						-- autoUseWorkspaceTsdk = true,
						-- tsdk = vim.fs.joinpath(vim.uv.cwd() .. 'node_modules/typescript/lib'),
						settings = {
							typescript = {
								inlayHints = {
									parameterNames = { enabled = 'literals' },
									parameterTypes = { enabled = true },
									variableTypes = { enabled = true },
									propertyDeclarationTypes = { enabled = true },
									functionLikeReturnTypes = { enabled = true },
									enumMemberValues = { enabled = true },
								},
								tsserver = {
									maxTsServerMemory = 8192,
									-- globalPlugins = {
									-- 	{
									-- 		name = '@styled/typescript-styled-plugin',
									-- 		location = vim.fn.expand '~/.volta/tools/image/packages/@styled/typescript-styled-plugin/lib/node_modules',
									-- 		enableForWorkspaceTypeScriptVersions = true,
									-- 	},
									-- },
								},
							},
						},
						experimental = {
							completion = {
								enableServerSideFuzzyMatch = true,
							},
						},
					},
					zls = {},
				},
				-- you can do any additional lsp server setup here
				-- return true if you don't want this server to be setup with lspconfig
				---@type table<string, fun(server:string, opts:_.lspconfig.options):boolean?>
				setup = {},
			}
		end,
		config = function(_, opts)
			U.lsp_attach('*', function(client, buf)
				-- NOTE: uncomment to see
				-- vim.print(client.server_capabilities)

				local reuse_win = true
				local on_list

				-- setup go to def (with filters for bits I never want)
				if client.name == 'lua_ls' then
					on_list = require('plugins.lsp.definition').on_list_fact {
						reuse_win = reuse_win,
						filter = function(item)
							--- WIP:
							--- - in lua class defs might be better refersed (extensions are more interesting)

							-- remove function assignments, e.g local foo = function() end
							local s = item.text:gsub('%s', ' ')
							local start = s:find 'function%s*%('
							return start ~= item.col
						end,
					}
				end

				--[[stylua: ignore]] --format
					Keys.list({buffer = true }, {
	{ 'gD', 'declaration'   , function() vim.lsp.buf.declaration() end                                                       },
	{ 'gd', 'definition'    , function() vim.lsp.buf.definition { on_list = on_list, reuse_win = reuse_win, } end            },
	{ 'K' , 'hover'         , function() if not require('ufo').peekFoldedLinesUnderCursor() then vim.lsp.buf.hover() end end },
	{ 'gi', 'implementation', function() vim.lsp.buf.implementation() end                                                    },
	{ 'gh', 'signature_help', function() vim.lsp.buf.signature_help() end                                                    },
	{ 'gr', 'references'    , function() vim.lsp.buf.references() end                                                        },
				})
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

			local servers = opts.servers

			local capabilities = vim.tbl_deep_extend(
				'force',
				{},
				vim.lsp.protocol.make_client_capabilities(),
				require('cmp_nvim_lsp').default_capabilities()
			)

			local function setup(server)
				local server_opts = vim.tbl_deep_extend('force', {
					capabilities = vim.deepcopy(capabilities),
				}, servers[server] or {})

				if opts.setup[server] then
					if opts.setup[server](server, server_opts) then return end
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
