if vim.g.vscode then return {} end

return U.F {
	{
		'mzlogin/vim-markdown-toc',
		init = function() vim.cmd [[let g:vmt_auto_update_on_save = 0]] end,
	},

	{
		'iamcco/markdown-preview.nvim',
		cmd = { 'MarkdownPreviewToggle', 'MarkdownPreview', 'MarkdownPreviewStop' },
		build = 'sh -c "cd app && yarn install"',
		init = function()
			-- vim.g.mkdp_browser = 'firefox'
			vim.g.mkdp_filetypes = { 'markdown' }
			vim.g.mkdp_refresh_slow = 1 -- too much jumping around, refresh on save or insert leave
		end,
		ft = { 'markdown' },
	},

	{
		'codethread/peek.nvim',
		branch = 'mermaids',
		build = 'deno task --quiet build:fast',
		opts = {
			-- app = 'browser',
		},
		init = function()
			vim.api.nvim_create_user_command('PeekOpen', function() require('peek').open() end, {})
			vim.api.nvim_create_user_command('PeekClose', function() require('peek').close() end, {})
		end,
	},

	{
		'MeanderingProgrammer/render-markdown.nvim',
		enabled = false,
		-- also 'OXY2DEV/markview.nvim',
		ft = 'markdown',
		---@module 'render-markdown'
		---@type render.md.UserConfig
		opts = {
			completions = { lsp = { enabled = true } },
		},
	},
}
