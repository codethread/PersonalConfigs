local function get_codelldb()
	local mason_registry = require 'mason-registry'
	local codelldb = mason_registry.get_package 'codelldb'
	local extension_path = codelldb:get_install_path() .. '/extension/'
	local codelldb_path = extension_path .. 'adapter/codelldb'
	local liblldb_path = extension_path .. 'lldb/lib/liblldb.so'
	return codelldb_path, liblldb_path
end

return {
	{
		'neovim/nvim-lspconfig',
		dependencies = {
			'simrat39/rust-tools.nvim',
			'rust-lang/rust.vim',
			-- 'nvim-dap'
		},
		ft = 'rust',
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

			U.keys('rust', {
				{ 'h', Lua "require('rust-tools').hover_actions.hover_actions()", 'hover actions' },
				{
					'a',
					Lua "require('rust-tools').code_action_group.code_action_group()",
					'code actions',
				},
				{ 'l', function() vim.lsp.codelens.run() end, 'Code Lens' },
				{ 'e', Cmd 'RustRunnables', 'Runnables' },
				{ 'd', Cmd 'RustDebuggables', 'Runnables' },
				{ 't', Cmd 'Cargo test', 'Cargo test' },
				{ 'R', Cmd 'Cargo run', 'Cargo run' },
				{
					'cc',
					Cmd '%g/println/norm dd',
					'Clear logs',
				},
			})
		end,
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
		event = { 'BufRead Cargo.toml' },
		init = function()
			vim.api.nvim_create_autocmd({ 'BufEnter' }, {
				pattern = { 'Cargo.toml' },
				callback = function(event)
					U.keys(event.buf, {
						{ 'y', Lua "require'crates'.open_repository()", 'Open Repository' },
						{ 'p', Lua "require'crates'.show_popup()", 'Show Popup' },
						{ 'i', Lua "require'crates'.show_crate_popup()", 'Show Info' },
						{ 'f', Lua "require'crates'.show_features_popup()", 'Show Features' },
						{
							'd',
							Lua "require'crates'.show_dependencies_popup()",
							'Show Dependencies',
						},
					})
				end,
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
