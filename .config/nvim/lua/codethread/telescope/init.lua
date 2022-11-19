local safe_load = require('codethread.utils').safe_load

local function ts_word()
	local winnr = vim.fn.winnr '#'
	local buf = vim.fn.winbufnr(winnr)
	local c = vim.fn.getcurpos(winnr)
	local row = c[2] - 1
	local col = c[3] - 1

	local node = vim.treesitter.get_node_at_pos(buf, row, col, {})
	local txt = vim.treesitter.get_node_text(node, buf, {})
	return txt
end

safe_load('telescope', function(telescope)
	local actions = require 'telescope.actions'
	local action_layout = require 'telescope.actions.layout'
	local t_themes = require 'telescope.themes'
	local my_themes = require 'codethread.telescope.themes'

	telescope.load_extension 'lsp_handlers'
	telescope.load_extension 'neoclip'

	telescope.setup {
		-- defaults = themes.get_ivy {
		defaults = my_themes.bottom {
			prompt_prefix = '   ',
			selection_caret = '  ',
			entry_prefix = '  ',
			initial_mode = 'insert',
			path_display = { 'truncate' },
			-- winblend = 0,
			-- borderchars = { '─', '│', '─', '│', '╭', '╮', '╯', '╰' },
			color_devicons = true,
			cycle_layout_list = {
				'horizontal',
				-- 'vertical',
				'center',
				-- 'cursor',
				-- 'flex',
				-- 'bottom_pane',
				my_themes.bottom {},
			},
			-- layout_strategy = 'horizontal',
			-- me
			file_ignore_patterns = {
				'^.git/',
				'^.yarn/',
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
			-- border = true,
			-- borderchars = { '─', '│', '─', '│', '┌', '┐', '┘', '└' },
			-- prompt = { "─", "│", " ", "│", "┌", "┐", "│", "│" },
			-- results = { "─", "│", "─", "│", "├", "┤", "┘", "└" },
			-- preview = { "─", "│", "─", "│", "┌", "┐", "┘", "└" },

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
		},
		pickers = {
			find_files = {
				hidden = true,
				find_command = { 'fd', '--type', 'f', '--strip-cwd-prefix' },
			},
			live_grep = {
				hidden = true,
			},
		},
		extensions = {
			lsp_handlers = {
				code_action = {
					telescope = require('telescope.themes').get_cursor {},
				},
			},
			['ui-select'] = {
				t_themes.get_ivy {},
			},
		},
	}
	telescope.load_extension 'notify'

	telescope.load_extension 'ui-select'
end)
