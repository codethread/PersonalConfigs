local M = {}

function M.bottom(opts)
	opts = opts or {}

	local theme_opts = {
		theme = 'bottom',
		sorting_strategy = 'ascending',
		layout_strategy = 'bottom_pane',
		layout_config = {
			height = function(_, _, max_lines) return math.min(max_lines, 15) end,
		},
		border = true,
		borderchars = {
			prompt = { '─', ' ', ' ', ' ', '─', '─', ' ', ' ' },
			results = { ' ' },
			preview = { '─', '│', '─', '│', '╭', '╮', '╯', '╰' },
		},
	}

	if opts.layout_config and opts.layout_config.prompt_position == 'bottom' then
		theme_opts.borderchars = {
			prompt = { ' ', ' ', '─', ' ', ' ', ' ', '─', '─' },
			results = { '─', ' ', ' ', ' ', '─', '─', ' ', ' ' },
			preview = { '─', ' ', '─', '│', '┬', '─', '─', '╰' },
		}
	end

	return vim.tbl_deep_extend('force', theme_opts, opts)
end

return M
