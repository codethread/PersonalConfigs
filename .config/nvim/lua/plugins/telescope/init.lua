return {
	{
		'nvim-telescope/telescope.nvim',
		cmd = 'Telescope',
		dependencies = {
			{ 'nvim-telescope/telescope-fzf-native.nvim', build = 'make' },
			{ 'nvim-telescope/telescope-live-grep-args.nvim' },
			{ 'catgoose/telescope-helpgrep.nvim' },
			U.highlights {
				TelescopeBorder = { fg = 'highlight_low', bg = 'highlight_low' },
				TelescopeNormal = { fg = 'text', bg = 'highlight_low' },
				TelescopeSelection = { fg = 'text', bg = 'highlight_med' },
				TelescopeSelectionCaret = { fg = 'love', bg = 'highlight_med' },
				TelescopeMultiSelection = { fg = 'text', bg = 'highlight_high' },

				TelescopeTitle = { fg = 'love' },
				TelescopePromptTitle = { fg = 'rose' },
				TelescopePreviewTitle = { fg = 'iris' },

				TelescopePromptNormal = { fg = 'text', bg = 'surface' },
				TelescopePromptBorder = { fg = 'surface', bg = 'surface' },
				TelescopePromptCounter = { fg = 'subtle' },

				-- alt
				-- TelescopeBorder = { fg = 'muted', bg = 'base' },
				-- TelescopeNormal = { fg = 'subtle', bg = 'base' },
				-- TelescopePromptCounter = { fg = 'subtle', bg = 'base' },
				-- TelescopePromptNormal = { fg = 'text', bg = 'base' },
				-- TelescopePromptBorder = { fg = 'rose', bg = 'base' },
			},
		},
		config = function(_, opts)
			opts = opts or {}
			local telescope = require 'telescope'
			local actions = require 'telescope.actions'
			local action_layout = require 'telescope.actions.layout'
			local custom = require 'plugins.telescope.pickers'

			local options = vim.tbl_deep_extend('force', opts, {
				defaults = {
					-- ui
					layout_strategy = 'flex',
					layout_config = {
						height = { padding = 1 },
						width = { padding = 2 },
						horizontal = {
							-- mirror = true,
							preview_cutoff = 160,
							preview_width = { 0.6, min = 80, max = 120 + 2 },
						},
						vertical = {
							width = { 0.99, max = 120 + 2 },
							mirror = true,
							preview_height = 0.4,
							preview_cutoff = 1,
						},
					},
					prompt_prefix = '   ',
					selection_caret = '  ',
					entry_prefix = '  ',
					initial_mode = 'insert',
					path_display = { 'truncate' },
					color_devicons = true,
					dynamic_preview_title = true,
					mappings = {
						i = {
							['<C-n>'] = actions.cycle_history_next,
							['<C-p>'] = actions.cycle_history_prev,

							['<C-j>'] = actions.move_selection_next,
							['<C-k>'] = actions.move_selection_previous,

							['<C-c>'] = actions.close,

							['<Down>'] = actions.move_selection_next,
							['<Up>'] = actions.move_selection_previous,

							['<CR>'] = actions.select_default,
							['<C-x>'] = actions.select_horizontal,
							['<C-v>'] = actions.select_vertical,
							['<C-t>'] = actions.select_tab,

							['<C-u>'] = actions.preview_scrolling_up,
							['<C-d>'] = actions.preview_scrolling_down,

							['<PageUp>'] = actions.results_scrolling_up,
							['<PageDown>'] = actions.results_scrolling_down,

							['<Tab>'] = actions.toggle_selection + actions.move_selection_worse,
							['<S-Tab>'] = actions.toggle_selection + actions.move_selection_better,
							['<C-q>'] = actions.send_to_qflist + actions.open_qflist,
							['<M-q>'] = actions.send_selected_to_qflist + actions.open_qflist,
							['<C-l>'] = actions.complete_tag,
							['<C-_>'] = actions.which_key, -- keys from pressing <C-/>
							['<M-p>'] = action_layout.toggle_preview,
							['<C-Space>'] = actions.to_fuzzy_refine, -- this is awesome

							['<C-i>'] = require('telescope-live-grep-args.actions').quote_prompt {
								postfix = ' --iglob ',
							},
						},

						n = {
							['<esc>'] = actions.close,
							['<CR>'] = actions.select_default,
							['<C-x>'] = actions.select_horizontal,
							['<C-v>'] = actions.select_vertical,
							['<C-t>'] = actions.select_tab,

							['<Tab>'] = actions.toggle_selection + actions.move_selection_worse,
							['<S-Tab>'] = actions.toggle_selection + actions.move_selection_better,
							['<C-q>'] = actions.send_to_qflist + actions.open_qflist,
							['<M-q>'] = actions.send_selected_to_qflist + actions.open_qflist,

							['j'] = actions.move_selection_next,
							['k'] = actions.move_selection_previous,
							['H'] = actions.move_to_top,
							['M'] = actions.move_to_middle,
							['L'] = actions.move_to_bottom,

							['<Down>'] = actions.move_selection_next,
							['<Up>'] = actions.move_selection_previous,
							['gg'] = actions.move_to_top,
							['G'] = actions.move_to_bottom,

							['<C-u>'] = actions.preview_scrolling_up,
							['<C-d>'] = actions.preview_scrolling_down,

							['<PageUp>'] = actions.results_scrolling_up,
							['<PageDown>'] = actions.results_scrolling_down,

							['?'] = actions.which_key,
							['<M-p>'] = action_layout.toggle_preview,

							['D'] = actions.delete_buffer, -- only for buffers, see if better way todo
							['>'] = action_layout.cycle_layout_next,
							[']'] = actions.cycle_previewers_next,

							['<M-l>'] = action_layout.toggle_mirror,
						},
					},

					-- these are lua patterns
					file_ignore_patterns = U.flatten {
						{
							'^.git/',
							'^.yarn/',
							-- '/vendor/',
							-- '%.lock',
						},
						U.project('~/dev/projects/qmk.nvim', { 'lua/qmk/lib/' }),
					},
					vimgrep_arguments = {
						'rg',
						'--color=never',
						'--no-heading',
						'--with-filename',
						'--line-number',
						'--column',
						'--smart-case',
						'--hidden',
					},
					cache_picker = {
						num_pickers = 2, -- store 2 in history for making nested pickers
					},
				},
				pickers = {
					find_files = {
						find_command = {
							'rg',
							'--hidden',
							'--glob',
							'!.git',
							'--files',
						},
					},
					live_grep = {
						hidden = true,
						glob_pattern = '!.git',
					},
					live_grep_args = {
						glob_pattern = '!.git',
					},
					diagnostics = {
						path_display = 'hidden',
					},
					oldfiles = {
						layout_config = { horizontal = { mirror = false } },
						prompt_title = 'Oldfiles (cwd)',
						only_cwd = true,
						attach_mappings = custom.builtin_oldfiles_toggle_cwd,
					},
					help_tags = {
						attach_mappings = function(prompt_bufnr)
							actions.select_default:replace(custom.action_open_help_vert(prompt_bufnr))
							return true
						end,
					},
				},
				extensions = {
					helpgrep = {
						ignore_paths = {
							vim.fn.stdpath 'state' .. '/lazy/readme',
						},
						-- mappings = {
						--   i = {
						--     ["<CR>"] = actions.select_default,
						--     ["<C-v>"] = actions.select_vertical,
						--   },
						--   n = {
						--     ["<CR>"] = actions.select_default,
						--     ["<C-s>"] = actions.select_horizontal,
						--   }
						-- },
						-- default_grep = builtin.live_grep,
					},
				},
			})

			telescope.setup(options)

			telescope.load_extension 'live_grep_args'
			telescope.load_extension 'fzf'
			telescope.load_extension 'helpgrep'
		end,
	},
}
