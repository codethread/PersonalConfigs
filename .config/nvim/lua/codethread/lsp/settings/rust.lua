-- return {
-- 	settings = {
-- 		['rust-analyzer'] = {
-- 			checkOnSave = {
-- 				allFeatures = true,
-- 				command = 'clippy',
-- 			},
-- 		},
-- 	},

-- 	on_attach = function(client, bufnr) require('codethread.lsp.settings.shared').on_attach(client, bufnr) end,
-- }
local rt = require 'rust-tools'

rt.setup {
	server = {
		on_attach = function(client, bufnr)
			require('codethread.lsp.settings.shared').on_attach(client, bufnr)
			-- Hover actions
			-- vim.keymap.set('n', '<C-space>', rt.hover_actions.hover_actions, { buffer = bufnr })
			-- Code action groups
			vim.keymap.set(
				'n',
				'<localleader>a',
				rt.code_action_group.code_action_group,
				{ buffer = bufnr }
			)
		end,
	},
}
