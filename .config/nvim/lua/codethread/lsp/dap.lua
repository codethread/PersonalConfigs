local hydras = require 'codethread.lsp.dap-hydra'
-- if having trouble with js try:
-- https://www.reddit.com/r/neovim/comments/y7dvva/typescript_debugging_in_neovim_with_nvimdap/?utm_source=share&utm_medium=ios_app&utm_name=iossmf
U.safe_load('dap', function(dap)
	-- setup repl automatic
	-- Completion will then trigger automatically on any of the completion trigger
	-- characters reported by the debug adapter, or on `.` if none are reported.
	vim.cmd [[
        au FileType dap-repl lua require('dap.ext.autocompl').attach()
	]]

	require('mason-nvim-dap').setup {
		ensure_installed = { 'node2' },
		automatic_installation = true,
		automatic_setup = true,
	}
	require('mason-nvim-dap').setup_handlers()

	dap.configurations.lua = {
		{
			type = 'nlua',
			request = 'attach',
			name = 'Attach to running Neovim instance',
		},
	}

	dap.adapters.nlua = function(callback, config)
		P(config)
		callback {
			type = 'server',
			host = config.host or '127.0.0.1',
			port = config.port or 8086,
		}
	end

	local dapui = require 'dapui'
	dapui.setup {}
	require('nvim-dap-virtual-text').setup {}

	dap.listeners.after.event_initialized['dapui_config'] = function()
		dapui.open()
		hydras.debug_running_hydra:activate()
	end
	dap.listeners.before.event_terminated['dapui_config'] = function()
		hydras.debug_running_hydra:exit()
		dapui.close()
	end
	dap.listeners.before.event_exited['dapui_config'] = function()
		hydras.debug_running_hydra:exit()
		dapui.close()
	end
end)
