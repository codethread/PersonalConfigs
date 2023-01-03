local u = require 'codethread.utils'
local nmap = u.nmap

local api = vim.api
local keymap_restore = {}

-- if having trouble with js try:
-- https://www.reddit.com/r/neovim/comments/y7dvva/typescript_debugging_in_neovim_with_nvimdap/?utm_source=share&utm_medium=ios_app&utm_name=iossmf

local safe_load = require('codethread.utils').safe_load

safe_load('dap', function(dap)
	require('mason-nvim-dap').setup {
		ensure_installed = { 'node2' },
		automatic_installation = true,
		automatic_setup = true,
	}
	require('mason-nvim-dap').setup_handlers()

	local dapui = require 'dapui'
	dapui.setup {}
	require('nvim-dap-virtual-text').setup {}

	-- local dap_nmap = dap_nmap_partial(dap)

	-- dap_nmap {
	-- 	{ 'K', '<Cmd>lua require("dap.ui.widgets").hover()<CR>' },
	-- }

	dap.listeners.after.event_initialized['dapui_config'] = function() dapui.open() end
	dap.listeners.before.event_terminated['dapui_config'] = function() dapui.close() end
	dap.listeners.before.event_exited['dapui_config'] = function() dapui.close() end
end)

local Hydra = require 'hydra'
local debug_hydra = Hydra {
	name = 'Debug',
	hint = [[
 ^ ^         Step^ ^              ^ ^Stack    ^ ^      Action
 ^-^-------------^-^-----------   ^-^-------  ^-^----------------------- 
 _j_: step over  _k_: step back   _n_: down   _c_: continue  
 _h_: step out   _l_: step into   _p_: up     _<Tab>_: toggle breakpoint
 ^ ^             ^ ^              ^ ^         _x_: terminate debugger
 ^ ^             ^ ^              ^ ^         _._: repeat last
 ^
 _q_: / _<Esc>_: close 
 ]],
	mode = 'n',
	-- body = '<leader>d',
	config = {
		color = 'pink',
		foreign_keys = nil,
		hint = {
			border = 'rounded',
			position = 'bottom-right',
			show_name = false,
		},
	},
	heads = {

		{ 'h', function() require('dap').step_back() end },
		{ 'j', function() require('dap').step_over() end },
		{ 'k', function() require('dap').step_out() end },
		{ 'l', function() require('dap').step_into() end },

		{ 'n', function() require('dap').down() end },
		{ 'p', function() require('dap').up() end },

		{ 'c', function() require('dap').continue() end },
		{ '<Tab>', function() require('dap').toggle_breakpoint() end },
		{ 'x', function() require('dap').terminate() end, { exit = true } },
		{ '.', function() require('dap').run_last() end },

		{ 'q', nil, { exit = true } },
		{ '<Esc>', nil, { exit = true } },
	},
}

return { debug_hydra = debug_hydra }
