---@diagnostic disable: missing-fields

return {
	'folke/snacks.nvim',
	dependencies = {
		U.highlights {
			SnacksDebugPrint = { bg = 'surface', fg = 'white' },
		},
	},
	priority = 1000,
	lazy = false,
	--[[stylua: ignore]] --format
	keys = {
	{ '<leader>.', function() Snacks.scratch() end       , desc = 'Toggle Scratch Buffer' },
	{ '<leader>S', function() Snacks.scratch.select() end, desc = 'Select Scratch Buffer' },
	},
	---@type snacks.Config
	---@diagnostic disable-next-line: missing-fields
	opts = vim.g.vscode and {
		bigfile = { enabled = false },
		dashboard = { enabled = false },
		indent = { enabled = false },
		input = { enabled = false },
		notifier = { enabled = false },
		picker = { enabled = false },
		quickfile = { enabled = false },
		scroll = { enabled = false },
		statuscolumn = { enabled = false },
	} or {
		bigfile = { enabled = true },
		quickfile = {},
		notifier = {
			-- NOTE: from 'rcarriga/nvim-notify',
			-- silence annoying errors from lsp's that have no hover information
			-- source https://github.com/neovim/nvim-lspconfig/issues/1931#issuecomment-1297599534
			-- local banned_messages = {
			-- 	'No information available',
			-- 	'warning: multiple different client offset_encodings detected for buffer, this is not supported yet',
			-- }
		},
		---@type snacks.dashboard.Config
		dashboard = {},
	},
}
