return {
	{
		'ibhagwan/fzf-lua',
		-- optional for icon support
		dependencies = { 'nvim-tree/nvim-web-devicons' },
		-- or if using mini.icons/mini.nvim
		-- dependencies = { "echasnovski/mini.icons" },
		opts = {},
	},
	{
		'nvim-telescope/telescope.nvim',
		version = '0.1.x',
		cmd = 'Telescope',
		dependencies = {
			{ 'nvim-telescope/telescope-fzf-native.nvim', build = 'make' },
			U.highlights {
				TelescopeBorder = { fg = 'highlight_high', bg = 'none' },
				TelescopeNormal = { bg = 'none' },
				TelescopePromptNormal = { bg = 'base' },
				TelescopeResultsNormal = { fg = 'subtle', bg = 'none' },
				TelescopeSelection = { fg = 'text', bg = 'surface' },
				TelescopeSelectionCaret = { fg = 'rose', bg = 'rose' },
			},
			{
				'nvim-telescope/telescope-live-grep-args.nvim',
				version = '^1.0.0',
			},
		},
		config = function(_, opts)
			local telescope = require 'telescope'
			local actions = require 'telescope.actions'
			local action_layout = require 'telescope.actions.layout'

			local options = vim.tbl_deep_extend('force', opts, {
				defaults = {
					-- ui
					prompt_prefix = '   ',
					selection_caret = '  ',
					entry_prefix = '  ',
					initial_mode = 'insert',
					path_display = { 'truncate' },
					color_devicons = true,
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

							['C-1'] = action_layout.toggle_prompt_position,
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
				},
				pickers = {
					find_files = {
						-- hidden = true,
						-- find_command = { 'fd', '--type', 'f', '--strip-cwd-prefix' },
						-- rg is actually faster at finding files!
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
				},
			})

			telescope.setup(options)

			telescope.load_extension 'live_grep_args'

			-- WIP
			--[[ local function ts_word()
	  -- local winnr = vim.fn.winnr '#'
	  local winnr = vim.fn.winnr()
	  local buf = vim.fn.winbufnr(winnr)
	  local c = vim.fn.getcurpos(winnr)
	  local row = c[2] - 1
	  local col = c[3] - 1

	  local ts_utils = require 'nvim-treesitter.ts_utils'
	  local node = ts_utils.get_node_at_cursor(winnr)
	  -- local node = vim.treesitter.get_node_at_pos(buf, row, col, {})
	  local txt = vim.treesitter.get_node_text(node, buf, {})
	  return txt
	end

	vim.api.nvim_create_user_command('TSWord', function() print(ts_word()) end, {}) ]]
		end,
	},
}
