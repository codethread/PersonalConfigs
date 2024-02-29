return {
	{
		'folke/which-key.nvim',
		lazy = false,
		priority = 100,
		---@type Options
		opts = {
			plugins = {
				presets = {
					operators = false,
					motions = false,
					text_objects = false,
				},
			},
			-- ignore_missing = true, -- enable this to hide mappings for which you didn't specify a label
			-- hidden = { '<silent>', '<Cmd>', '', 'call', 'lua', '^:', '^ ' }, -- hide mapping boilerplate
		},
		config = function(_, opts)
			local wk = require 'which-key'
			wk.setup(opts)
			wk.register(require 'plugins.keymaps.leader', {
				mode = 'n', -- NORMAL mode
				prefix = '<leader>',
				buffer = nil, -- Global mappings. Specify a buffer number for buffer local mappings
				silent = true, -- use `silent` when creating keymaps
				noremap = true, -- use `noremap` when creating keymaps
				nowait = true, -- use `nowait` when creating keymaps
			})

			wk.register(require 'plugins.keymaps.normal', {
				mode = 'n', -- NORMAL mode
				buffer = nil, -- Global mappings. Specify a buffer number for buffer local mappings
				silent = true, -- use `silent` when creating keymaps
				noremap = true, -- use `noremap` when creating keymaps
				nowait = true, -- use `nowait` when creating keymaps
			})

			wk.register({
				s = {
					name = 'Search',
					s = {
						function()
							require('telescope-live-grep-args.shortcuts').grep_visual_selection()
						end,
						'live',
					},
					r = {
						function() require('spectre').open_visual { select_word = true } end,
						'find-replace',
					},
				},
			}, {
				mode = 'v',
				prefix = '<leader>',
				buffer = nil, -- Global mappings. Specify a buffer number for buffer local mappings
				silent = true, -- use `silent` when creating keymaps
				noremap = true, -- use `noremap` when creating keymaps
				nowait = true, -- use `nowait` when creating keymaps
			})
		end,
	},
}
