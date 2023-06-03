return {
	{
		'neovim/nvim-lspconfig',
		ft = 'go',
		dependencies = {
			{ 'ray-x/go.nvim', dependencies = 'ray-x/guihua.lua' },
		},
		opts = {
			servers = {
				gopls = {},
			},
			setup = {
				gopls = function(_, opts)
					require('go').setup {
						dap_debug_keymap = false,
						lsp_cfg = opts,
					}

					local Term = require('toggleterm.terminal').Terminal

					local go_run = Term:new {
						cmd = 'go run .',
						hidden = true,
						close_on_exit = false,
						direction = 'vertical',
					}

					U.keys('go', {
						{ 'r', function() return vim.cmd.wa() and go_run:toggle() end, 'GoRun' },
						{ 'e', Cmd 'GoIfErr', 'err' },
					})

					return true
				end,
			},
		},
	},
}
