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
			key_labels = {
				-- override the label used to display some keys. It doesn't effect WK in any other way.
				-- For example:
				['<space>'] = '<SPC>',
				['<cr>'] = '<RET>',
				['<tab>'] = '<TAB>',
			},
			window = {
				border = 'rounded', -- none, single, double, shadow
				position = 'bottom', -- bottom, top
				margin = { 1, 0, 1, 0 }, -- extra window margin [top, right, bottom, left]
				padding = { 2, 2, 2, 2 }, -- extra window padding [top, right, bottom, left]
				winblend = 0,
			},
			-- ignore_missing = true, -- enable this to hide mappings for which you didn't specify a label
			-- hidden = { '<silent>', '<Cmd>', '', 'call', 'lua', '^:', '^ ' }, -- hide mapping boilerplate
			defaults = {
				[']'] = { name = '+next' },
				['['] = { name = '+prev' },
				-- ['<leader>v'] = { name = '+vim' },
				[';'] = { Cmd 'Telescope commands', 'M-x' },
				[':'] = { Cmd 'Telescope command_history', 'M-x [hist]' },
				['/'] = { Cmd 'Telescope search_history', '/ [hist]' },
				['q'] = { Cmd 'w | luafile %', 'Reload Luafile' },

				-- buffers, buffer
				b = {
					name = 'Buffers',
					b = { [[<C-^>]], 'Toggle' },
					l = { Cmd 'Telescope buffers', 'list' },
					r = { Cmd 'Telescope oldfiles', 'recent' },
					k = { Cmd 'Bdelete', 'kill' },
					s = { Cmd 'w', 'Save' },
				},

				e = { name = 'Errors' },

				-- d = {
				--   function()
				--     -- TODO: potentiall save all buffers first
				--     local dap = require 'dap'
				--     if dap.session() then
				--       require('codethread.lsp.dap-hydra').debug_running_hydra:activate()
				--     else
				--       require('codethread.lsp.dap-hydra').debug_hydra:activate()
				--     end
				--   end,
				--   '🐉 Debug',
				-- },

				f = {
					name = 'Fold?',
					f = { 'za', 'toggle' },
				},

				j = {
					name = 'Test',
					j = {
						function()
							local ft = vim.ct.ft()
							if ft == 'lua' then
								vim.Cmd 'w'
								-- require('plenary.test_harness').test_directory(vim.fn.expand '%:p')
								require('plenary.test_harness').test_directory(
									vim.fn.expand '%:p',
									-- if writing lua tests, I'll follow the same setup as https://github.com/m00qek/plugin-template.nvim
									{ minimal_init = 'test/spec.vim' }
								)
								-- also PlenaryTestFile
							elseif ft == 'javascript' then
								local jest = require 'jester'
								jest.run_last { path_to_jest = './node_modules/bin/jest' }
							elseif ft == 'go' then
								vim.Cmd.GoTestFunc()
							else
								print('no setup for filetype: ' .. ft)
							end
						end,
						'file',
					},
				},

				g = {
					name = 'Git',
					R = {
						function() require('gitsigns').reset_buffer() end,
						'Reset Buffer',
					},
					b = { Cmd 'Telescope git_branches', 'Checkout branch' },
					c = { Cmd 'Telescope git_commits', 'Checkout commit' },
					d = { Cmd 'Gitsigns diffthis HEAD', 'Diff' },
					g = { function() require('neogit').open() end, 'Status' },
					j = { function() require('gitsigns').next_hunk() end, 'Next Hunk' },
					k = { function() require('gitsigns').prev_hunk() end, 'Prev Hunk' },
					l = { function() require('gitsigns').blame_line() end, 'Blame' },
					o = { Cmd 'Telescope git_status', 'Open changed file' },
					p = { function() require('gitsigns').preview_hunk() end, 'Preview Hunk' },
					r = { function() require('gitsigns').reset_hunk() end, 'Reset Hunk' },
					s = { function() require('gitsigns').stage_hunk() end, 'Stage Hunk' },
					u = { function() require('gitsigns').undo_stage_hunk() end, 'Undo Stage Hunk' },
					v = { Cmd 'silent !gh repo view --web', 'Ghub view' },
				},

				G = {
					name = 'Global',
					z = { Cmd 'Lazy', 'Lazy' },
				},

				h = {
					name = 'Help',
					h = { Cmd 'Telescope help_tags', 'Help' },
					m = { Cmd 'Telescope man_pages', 'Man' },
					v = { Cmd 'Telescope vim_options', 'Settings' },
					t = { Cmd 'Telescope builtin', 'Telescope' },
					c = { Cmd 'Telescope highlights', 'Telescope' },
				},

				l = {
					name = 'LSP',
					-- a = { Cmd'Telescope lsp_code_actions them=cursor', "Code Action" },
					a = {
						Cmd 'lua vim.lsp.buf.code_action()',
						'Code Action',
					},
					i = { Cmd 'LspInfo', 'Info' },
					I = { Cmd 'LspInstallInfo', 'Installer Info' },
					l = { Cmd 'lua vim.lsp.codelens.run()', 'CodeLens Action' },
					r = { Cmd 'lua vim.lsp.buf.rename()', 'Rename' },
					s = { Cmd 'Telescope lsp_document_symbols', 'Document Symbols' },
					S = { Cmd 'Telescope lsp_dynamic_workspace_symbols', 'Workspace Symbols' },
				},

				['m'] = {
					function() require('codethread.movement').mover_hydra:activate() end,
					'🐉 Mover',
				},

				w = {
					name = 'Window',
					N = { Cmd 'tabnew', 'New Tab' },
					k = { Cmd 'close', 'Close' },
					l = { function() require('telescope-tabs').list_tabs() end, 'List Tabs' }, -- TODO: put through telescope
					n = { Cmd 'tabNext', 'Next Tab' }, -- TODO: put through telescope
					p = { Cmd 'tabprevious', 'Previous Tab' }, -- TODO: put through telescope
					w = { Cmd 'vsplit', 'Split' }, -- TODO: put through telescope
					s = { Cmd 'SwapSplit', 'Swap' },
					e = {
						function() require('codethread.movement').tab_hydra:activate() end,
						'🐉 Tabs',
					},
				},

				s = {
					name = 'Search',
					b = { Cmd 'Telescope git_branches', 'Checkout branch' },
					c = { Cmd 'Telescope colorscheme', 'Colorscheme' },
					f = { Cmd 'Telescope current_buffer_fuzzy_find theme=ivy previewer=false', 'Buffer' },
					h = { Cmd 'Telescope help_tags', 'Find Help' },
					M = { Cmd 'Telescope man_pages', 'Man Pages' },
					r = { Cmd 'Telescope oldfiles', 'Open Recent File' },
					R = { Cmd 'Telescope registers', 'Registers' },
					k = { Cmd 'Telescope keymaps', 'Keymaps' },
					C = { Cmd 'Telescope commands', 'Commands' },
					p = { Cmd 'Telescope live_grep', 'Live Grep' },
					w = { Cmd 'Telescope grep_string', 'Word' },
					o = { Cmd 'Telescope aerial', 'Symbol' },
				},

				-- open, Open, openers, Openers
				o = {
					name = 'Open',
					i = { Cmd 'OpenInitBuffer', 'Open init buffer' },
					-- if you can't beat 'em
					c = { Cmd 'silent !code %', 'VSCode' },
				},

				t = {
					name = 'Terminal',
					n = { Cmd 'lua _NODE_TOGGLE()', 'Node' },
					t = { Cmd 'ToggleTerm direction=float', 'Float' },
					h = { Cmd 'ToggleTerm size=10 direction=horizontal', 'Horizontal' },
					v = { Cmd 'ToggleTerm size=80 direction=vertical', 'Vertical' },
					r = { Cmd 'TermExec Cmd="eslint_d restart"', 'Vertical' },
				},

				T = {
					name = 'Toggle',
					i = { Cmd "lua print'nothing setup'", 'Inlay Hints' },
				},

				u = {
					name = 'Utils',
					f = { "mbggVG=='b", 'format buffer' },
				},
			},
		},
		config = function(_, opts)
			local wk = require 'which-key'
			wk.setup(opts)
			wk.register(opts.defaults, {
				mode = 'n', -- NORMAL mode
				prefix = '<leader>',
				buffer = nil, -- Global mappings. Specify a buffer number for buffer local mappings
				silent = true, -- use `silent` when creating keymaps
				noremap = true, -- use `noremap` when creating keymaps
				nowait = true, -- use `nowait` when creating keymaps
			})
		end,
	},
}
