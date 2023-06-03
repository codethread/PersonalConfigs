return {
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
		},
		keys = {
			{ '<leader><leader>', '<cmd>Telescope find_files<cr>', desc = 'Files' },
			{ '<leader>bl', '<cmd>Telescope buffers<cr>', desc = 'Buffers' },
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

					file_ignore_patterns = { '^.git/', '^.yarn/', '/vendor/' },
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
						hidden = true,
						find_command = { 'fd', '--type', 'f', '--strip-cwd-prefix' },
					},
					live_grep = {
						hidden = true,
					},
					diagnostics = {
						path_display = 'hidden',
					},
				},
			})

			telescope.setup(options)

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
