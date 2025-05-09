local js_based_languages = {
	'typescript',
	'javascript',
	'typescriptreact',
	'javascriptreact',
}
return {
	{
		'mfussenegger/nvim-dap',
		-- enabled = false, -- come back to this one day
		lazy = false,
		dependencies = {
			{
				'microsoft/vscode-js-debug',
				-- After install, build it and rename the dist directory to out
				build = 'npm install --legacy-peer-deps --no-save ; npx gulp vsDebugServerBundle ; rm -rf out ; mv dist out',
				version = '1.*',
			},
			{
				'mxsdev/nvim-dap-vscode-js',
				config = function()
					---@diagnostic disable-next-line: missing-fields
					require('dap-vscode-js').setup {
						-- Path of node executable. Defaults to $NODE_PATH, and then "node"
						-- node_path = "node",

						-- Path to vscode-js-debug installation.
						debugger_path = vim.fn.resolve(vim.fn.stdpath 'data' .. '/lazy/vscode-js-debug'),

						-- Command to use to launch the debug server. Takes precedence over "node_path" and "debugger_path"
						-- debugger_cmd = { "js-debug-adapter" },

						-- which adapters to register in nvim-dap
						adapters = {
							'chrome',
							'pwa-node',
							'pwa-chrome',
							-- 'pwa-msedge',
							-- 'pwa-extensionHost',
							-- 'node-terminal',
						},

						-- Path for file logging
						-- log_file_path = "(stdpath cache)/dap_vscode_js.log",

						-- Logging level for output to file. Set to false to disable logging.
						-- log_file_level = false,

						-- Logging level for output to console. Set to false to disable console output.
						-- log_console_level = vim.log.levels.ERROR,
					}
				end,
			},
			{
				'rcarriga/nvim-dap-ui',
				dependencies = {
					'mfussenegger/nvim-dap',
					'nvim-neotest/nvim-nio',
				},
				keys = {
					{ '<leader>du', function() require('dapui').toggle {} end, desc = 'Dap UI' },
					{
						'<leader>de',
						function() require('dapui').eval() end,
						desc = 'Eval',
						mode = { 'n', 'v' },
					},
				},
				opts = {},
				config = function(_, opts)
					local dap = require 'dap'
					local dapui = require 'dapui'
					dapui.setup(opts)
					dap.listeners.after.event_initialized['dapui_config'] = function() dapui.open {} end
					dap.listeners.before.event_terminated['dapui_config'] = function() dapui.close {} end
					dap.listeners.before.event_exited['dapui_config'] = function() dapui.close {} end
				end,
			},
			{ 'theHamsta/nvim-dap-virtual-text', opts = {} },
			{
				'jay-babu/mason-nvim-dap.nvim',
				dependencies = 'williamboman/mason.nvim',
				cmd = { 'DapInstall', 'DapUninstall' },
				opts = {
					-- Makes a best effort to setup the various debuggers with
					-- reasonable debug configurations
					automatic_installation = true,

					-- You can provide additional configuration to the handlers,
					-- see mason-nvim-dap README for more information
					handlers = {},

					-- You'll need to check that you have the required things installed
					-- online, please don't ask me how to install them :)
					ensure_installed = {
						'node2',
					},
					automatic_setup = true,
				},
				config = function(_, opts) require('mason-nvim-dap').setup(opts) end,
			},
			{
				'jbyuki/one-small-step-for-vimkind',
				keys = {
					{
						'<leader>daL',
						function() require('osv').launch { port = 8086 } end,
						desc = 'Adapter Lua Server',
					},
					-- {
					-- 	'<leader>dal',
					-- 	function() require('osv').run_this() end,
					-- 	desc = 'Adapter Lua',
					-- },
				},
				config = function()
					local dap = require 'dap'
					dap.adapters.nlua = function(callback, config)
						callback {
							type = 'server',
							host = config.host or '127.0.0.1',
							port = config.port or 8086,
						}
					end
					dap.configurations.lua = {
						{
							type = 'nlua',
							request = 'attach',
							name = 'Attach to running Neovim instance',
						},
					}
				end,
			},
		},
		keys = {
			{
				'<leader>dB',
				function() require('dap').set_breakpoint(vim.fn.input 'Breakpoint condition: ') end,
				desc = 'Breakpoint Condition',
			},
			{
				'<leader>db',
				function() require('dap').toggle_breakpoint() end,
				desc = 'Toggle Breakpoint',
			},
			{
				'<leader>dc',
				function()
					-- if vim.fn.filereadable '.vscode/launch.json' then
					-- 	local dap_vscode = require 'dap.ext.vscode'
					-- 	dap_vscode.load_launchjs(nil, {
					-- 		['pwa-node'] = js_based_languages,
					-- 		['chrome'] = js_based_languages,
					-- 		['pwa-chrome'] = js_based_languages,
					-- 	})
					-- end
					require('dap').continue()
				end,
				desc = 'Continue',
			},
			{
				'<leader>dC',
				function() require('dap').run_to_cursor() end,
				desc = 'Run to Cursor',
			},
			{
				'<leader>dg',
				function() require('dap').goto_() end,
				desc = 'Go to line (no execute)',
			},
			{
				'<leader>di',
				function() require('dap').step_into() end,
				desc = 'Step Into',
			},
			{
				'<leader>dj',
				function() require('dap').down() end,
				desc = 'Down',
			},
			{
				'<leader>dk',
				function() require('dap').up() end,
				desc = 'Up',
			},
			{
				'<leader>dl',
				function() require('dap').run_last() end,
				desc = 'Run Last',
			},
			{
				'<leader>do',
				function() require('dap').step_out() end,
				desc = 'Step Out',
			},
			{
				'<leader>dO',
				function() require('dap').step_over() end,
				desc = 'Step Over',
			},
			{
				'<leader>dp',
				function() require('dap').pause() end,
				desc = 'Pause',
			},
			{
				'<leader>dr',
				function() require('dap').repl.toggle() end,
				desc = 'Toggle REPL',
			},
			{
				'<leader>ds',
				function() require('dap').session() end,
				desc = 'Session',
			},
			{
				'<leader>dt',
				function() require('dap').terminate() end,
				desc = 'Terminate',
			},
			{
				'<leader>dw',
				function() require('dap.ui.widgets').hover() end,
				desc = 'Widgets',
			},
		},
		config = function()
			-- setup repl automatic
			-- Completion will then trigger automatically on any of the completion trigger
			-- characters reported by the debug adapter, or on `.` if none are reported.
			vim.cmd [[ au FileType dap-repl lua require('dap.ext.autocompl').attach() ]]

			local dap = require 'dap'

			for _, language in ipairs(js_based_languages) do
				dap.configurations[language] = {
					-- Debug single nodejs files
					{
						type = 'pwa-node',
						request = 'launch',
						name = 'Launch file',
						program = '${file}',
						cwd = vim.fn.getcwd(),
						sourceMaps = true,
					},
					-- Debug nodejs processes (make sure to add --inspect when you run the process)
					{
						type = 'pwa-node',
						request = 'attach',
						name = 'Attach',
						processId = require('dap.utils').pick_process,
						cwd = vim.fn.getcwd(),
						sourceMaps = true,
					},
					-- Debug web applications (client side)
					{
						type = 'pwa-chrome',
						request = 'launch',
						name = 'Launch & Debug Chrome',
						url = function()
							local co = coroutine.running()
							return coroutine.create(function()
								vim.ui.input(
									{
										prompt = 'Enter URL: ',
										default = 'http://localhost:3000',
									}, --
									function(url)
										if url == nil or url == '' then
											return
										else
											coroutine.resume(co, url)
										end
									end
								)
							end)
						end,
						webRoot = vim.fn.getcwd(),
						protocol = 'inspector',
						sourceMaps = true,
						userDataDir = false,
					},
					-- Divider for the launch.json derived configs
					{
						name = '----- ↓ launch.json configs ↓ -----',
						type = '',
						request = 'launch',
					},
				}
			end
		end,
	},
}
