-- concat a list of lists into a single list
local function list_concat(list)
	local out = {}

	for _, l in ipairs(list) do
		for _, v in ipairs(l) do
			table.insert(out, v)
		end
	end
	return out
end

vim.api.nvim_create_autocmd('FileType', {
	desc = "Easy quit dap-float with 'q'",
	group = vim.api.nvim_create_augroup('Floater', { clear = true }),
	pattern = 'dap-float',
	callback = function(o)
		vim.schedule(function() vim.keymap.set('n', 'q', '<cmd>q<cr>', { buffer = o.buf }) end)
	end,
})

local Hydra = require 'hydra'

local hint_config = {
	border = 'rounded',
	position = 'bottom-right',
	show_name = true,
	-- type = 'window',
}

local debug_shared = {
	{ 'q', nil, { exit = true } },
	{ '<Esc>', nil, { exit = true } },
}

-- HACK
local running_hydra = {}

local debug_actions_hydra = Hydra {
	name = 'Debug <Actions>',
	mode = 'n',
	config = {
		color = 'pink',
		foreign_keys = nil,
		hint = hint_config,
	},
	heads = list_concat {
		{
			{
				'<Tab>',
				function() running_hydra[1]:activate() end,
				{ desc = 'back', exit = true },
			},
			{
				'.',
				function() require('dap').run_last() end,
				{ desc = 'repeat last' },
			},
			{
				'x',
				function() require('dap').terminate() end,
				{ exit = true, desc = 'terminate' },
			},
			{
				'c',
				function() require('dap').clear_breakpoints() end,
				{ exit = true, desc = 'clear breakpoints' },
			},
			{
				'b',
				function() require('dap').set_breakpoint(vim.fn.input 'Breakpoint condition: ') end,
				{ exit = true, desc = 'breakpoint condition' },
			},
			{
				'm',
				function() require('dap').set_breakpoint(nil, nil, vim.fn.input 'Log point message: ') end,
				{ exit = true, desc = 'breakpoint log' },
			},
			{
				'l',
				function()
					-- true to open quickfix list, not sure why this is needed
					-- it doesn't do anything without it, far as i can tell
					require('dap').list_breakpoints(true)
				end,
				{ desc = 'list' },
			},
			{
				't',
				function() require('dapui').toggle() end,
				{ desc = 'list' },
			},
		},
		debug_shared,
	},
}

local debug_running_hydra = Hydra {
	name = 'Debugging',
	-- hint = [[
	-- ^ ^         Step^ ^              ^ ^Shifted    ^ ^      Action
	-- ^-^-------------^-^-----------   ^-^-------  ^-^-----------------------
	-- ^^^^ ↓ step over  ↑ step back   _n_: down
	-- ^^^^ ← step out   → step into   _p_: up     _<Tab>_: Actions
	-- ^ ^             ^ ^              ^ ^         _x_: terminate debugger
	-- ^
	-- _q_: / _<Esc>_: close
	-- ]],
	mode = 'n',
	-- body = '<leader>d',
	config = {
		color = 'pink',
		foreign_keys = nil,
		hint = hint_config,
		on_enter = function()
			local dap = require 'dap'
			local bp = require 'dap.breakpoints' -- XXX: internal
			-- add a breakpoint if non exist as this is usually my intent
			if #bp.get() == 0 then dap.toggle_breakpoint() end

			-- start debugging if not already running
			if not dap.session() then dap.continue() end
		end,
	},
	heads = list_concat {
		debug_shared,
		{
			{
				'<Up>',
				function() require('dap').step_back() end,
				{ desc = 'step_back' },
			},
			{
				'<Down>',
				function() require('dap').step_over() end,
				{ desc = 'step_over' },
			},
			{
				'<Left>',
				function() require('dap').step_out() end,
				{ desc = 'step_out' },
			},
			{
				'<Right>',
				function() require('dap').step_into() end,
				{ desc = 'step_into' },
			},

			{
				'<S-Down>',
				function() require('dap').down() end,
				{ desc = 'down' },
			},
			{
				'<S-Up>',
				function() require('dap').up() end,
				{ desc = 'up' },
			},
			{
				'<S-Right>',
				function() require('dap').continue() end,
				{ desc = 'continue' },
			},
			{
				'<S-Left>',
				function() require('dap').toggle_breakpoint() end,
				{ desc = 'toggle_breakpoint' },
			},

			{
				'<Tab>',
				function() debug_actions_hydra:activate() end,
				{ exit = true, desc = 'actions' },
			},
			{
				'K',
				function() require('dap.ui.widgets').hover() end,
				{ exit = true, desc = 'dap hover' },
			},
			{
				'x',
				function() require('dap').terminate() end,
				{ exit = true, desc = 'terminate' },
			},
		},
	},
}

local debug_hydra = Hydra {
	name = 'Debug',
	mode = 'n',
	config = {
		color = 'pink',
		foreign_keys = nil,
		hint = hint_config,
	},
	heads = list_concat {
		{
			{
				's',
				function()
					local dap = require 'dap'
					local bp = require 'dap.breakpoints' -- XXX: internal
					-- add a breakpoint if none exist as this is usually my intent
					-- TODO: doesn't always work as expected
					if #bp.get() == 0 then dap.toggle_breakpoint() end
					if vim.ct.ft() == 'lua' then
						-- literally runs current file
						-- to debug a process use launch
						require('osv').run_this()
					else
						dap.continue()
					end
				end,
				{ desc = 'start', exit = true },
			},
			{
				'd',
				function() require('dap').toggle_breakpoint() end,
				{ desc = 'toggle_breakpoint' },
			},
			{
				'.',
				function() require('dap').run_last() end,
				{ desc = 'run_last' },
			},
			{
				'l',
				function()
					-- true to open quickfix list, not sure why this is needed
					-- it doesn't do anything without it, far as i can tell
					require('dap').list_breakpoints(true)
				end,
				{ desc = 'list_breakpoints' },
			},
			{
				'L',
				function()
					if not vim.ct.ft() == 'lua' then return end

					require('osv').launch { port = 8086 }
					vim.notify 'run dap.continue() in target neovim, add a breakpoint, then come back here and :luafile file/to/run'
				end,
				{ desc = 'Launch Lua', exit = true },
			},
			{
				'b',
				function() require('dap').set_breakpoint(vim.fn.input 'Breakpoint condition: ') end,
				{ exit = true, desc = 'breakpoint condition' },
			},
			{
				'm',
				function() require('dap').set_breakpoint(nil, nil, vim.fn.input 'Log point message: ') end,
				{ exit = true, desc = 'breakpoint log' },
			},
		},
		debug_shared,
	},
}

running_hydra[1] = debug_running_hydra

return {
	debug_running_hydra = debug_running_hydra,
	debug_hydra = debug_hydra,
}
