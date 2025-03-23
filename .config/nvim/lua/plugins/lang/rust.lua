local function get_codelldb()
	local mason_registry = require 'mason-registry'
	local codelldb = mason_registry.get_package 'codelldb'
	local extension_path = codelldb:get_install_path() .. '/extension/'
	local codelldb_path = extension_path .. 'adapter/codelldb'
	local liblldb_path = extension_path .. 'lldb/lib/liblldb.so'
	return codelldb_path, liblldb_path
end

-- interesting stuff here to steal
-- https://github.com/noib3/dotfiles/blob/master/modules/home/neovim/lua/diagnostic/rust.lua

return U.F {
	{
		'neovim/nvim-lspconfig',
		dependencies = {
			'rust-lang/rust.vim', -- why here?
			{
				'simrat39/rust-tools.nvim',
				init = function()
					U.lsp_attach('rust_analyzer', function(client, bufnr)
						if client.supports_method 'textDocument/codeLens' then
							local rt = require 'rust-tools'
							rt.inlay_hints.enable()

							vim.api.nvim_create_autocmd(
								{ 'BufWritePost', 'BufEnter', 'CursorHold', 'InsertLeave' },
								{
									buffer = bufnr,
									callback = function() vim.lsp.codelens.refresh() end,
								}
							)
						end
					end)

			--[[stylua: ignore]] --format
			Keys.localleader_ft('rust', {
	{ 'h' , 'hover actions', Lua "require('rust-tools').hover_actions.hover_actions()"         },
	{ 'a' , 'code actions' , Lua "require('rust-tools').code_action_group.code_action_group()" },
	{ 'l' , 'Code Lens'    , function() vim.lsp.codelens.run() end                             },
	{ 'e' , 'Runnables'    , Cmd 'RustRunnables'                                               },
	{ 'd' , 'Runnables'    , Cmd 'RustDebuggables'                                             },
	{ 't' , 'Cargo test'   , Cmd 'Cargo test'                                                  },
	{ 'R' , 'Cargo run'    , Cmd 'Cargo run'                                                   },
	{ 'cc', 'Clear logs'   , Cmd '%g/println/norm dd'                                          },
			})
				end,
			},
			-- 'nvim-dap'
		},
		ft = 'rust',
		opts = {
			servers = {
				rust_analyzer = {
					settings = {
						['rust-analyzer'] = {
							cargo = { allFeatures = true },
							check = {
								extraArgs = {
									'--target-dir',
									'target/check',
								},
							},
						},
					},
				},
			},
			setup = {
				rust_analyzer = function(_, opts)
					local codelldb_path, liblldb_path = get_codelldb()

					require('rust-tools').setup {
						tools = {
							hover_actions = { border = 'solid' },
						},
						server = opts,
						-- dap = {
						-- 	adapter = require('rust-tools.dap').get_codelldb_adapter(
						-- 		codelldb_path,
						-- 		liblldb_path
						-- 	),
						-- },
					}

					-- the adapter inside rusttools expects rt_lldb, despite us changing it to codelldb
					-- hopefully gets fixed but for now this works
					-- local a = require('dap').adapters
					-- require('dap').adapters.rt_lldb = a.codelldb

					return true
				end,
			},
		},
	},

	{
		'saecki/crates.nvim',
		enabled = false,
		event = { 'BufRead Cargo.toml' },
		init = function()
			--stylua: ignore
			Keys.localleader_pat('Cargo.toml', { --
				{ 'y', 'Open Repository', Lua "require'crates'.open_repository()" },
				{ 'p', 'Show Popup', Lua "require'crates'.show_popup()" },
				{ 'i', 'Show Info', Lua "require'crates'.show_crate_popup()" },
				{ 'f', 'Show Features', Lua "require'crates'.show_features_popup()" },
				{ 'd', 'Show Dependencies', Lua "require'crates'.show_dependencies_popup()" },
			})
		end,
		opts = {
			null_ls = {
				enabled = true,
				name = 'crates.nvim',
			},
			popup = {
				border = 'rounded',
			},
		},
	},

	-- {
	-- 	'mfussenegger/nvim-dap',
	-- 	opts = {
	-- 		setup = {
	-- 			codelldb = function()
	-- 				local codelldb_path, _ = get_codelldb()
	-- 				local dap = require 'dap'
	-- 				dap.adapters.codelldb = {
	-- 					type = 'server',
	-- 					port = '${port}',
	-- 					executable = {
	-- 						command = codelldb_path,
	-- 						args = { '--port', '${port}' },
	--
	-- 						-- On windows you may have to uncomment this:
	-- 						-- detached = false,
	-- 					},
	-- 				}
	-- 				dap.configurations.cpp = {
	-- 					{
	-- 						name = 'Launch file',
	-- 						type = 'codelldb',
	-- 						request = 'launch',
	-- 						program = function()
	-- 							return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
	-- 						end,
	-- 						cwd = '${workspaceFolder}',
	-- 						stopOnEntry = false,
	-- 					},
	-- 				}
	--
	-- 				dap.configurations.c = dap.configurations.cpp
	-- 				dap.configurations.rust = dap.configurations.cpp
	-- 			end,
	-- 		},
	-- 	},
	-- },
}
