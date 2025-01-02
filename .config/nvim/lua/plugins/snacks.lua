return {
	'folke/snacks.nvim',
	priority = 1000,
	lazy = false,
	---@module "snacks"
	---@type snacks.Config
	---@diagnostic disable-next-line: missing-fields
	opts = {
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
	},
}
