local Term = require('toggleterm.terminal').Terminal

local clippy =
	Term:new { cmd = 'cargo clippy', hidden = true, close_on_exit = false, direction = 'vertical' }

local rt = require 'rust-tools'
U.wk('rust', {
	c = {
		function()
			vim.cmd.wa()
			clippy:toggle()
		end,
		'Clippy',
	},
	h = {
		function() rt.hover_actions.hover_actions() end,
		'hover',
	},
	a = {
		function() rt.code_action_group.code_action_group() end,
		'hover',
	},
})

rt.setup {
	server = {
		on_attach = function(client, bufnr)
			require('codethread.lsp.settings.shared').on_attach(client, bufnr)
		end,
	},
}
-- return {
-- 	settings = {
-- 		['rust-analyzer'] = {
-- 			checkOnSave = {
-- 				allFeatures = true,
-- 				command = 'clippy',
-- 			},
-- 		},
-- 	},

-- 	on_attach = function(client, bufnr)
-- 		require('codethread.lsp.settings.shared').on_attach(client, bufnr)
-- 	end,
-- }
