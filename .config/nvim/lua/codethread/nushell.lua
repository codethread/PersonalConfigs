local M = {}

local function send_to_pane(text, opts)
	opts = opts or {
		comment_character = '#',
	}
	local lines = type(text) == 'string' and vim.split(text, '\n', { plain = true }) or text

	-- escape charactes tmux things are special
	-- include literal ENTER to create new lines
	local tmux_text = vim.tbl_map(function(line)
		local escaped = string.gsub(line, '([%$"])', '\\%1')
		-- ; at end of line needs escaping, but not elswhere
		escaped = string.gsub(escaped, ';$', '\\;')
		return { '"' .. escaped .. '"', '"ENTER"' }
	end, lines)

	-- remove empty lines and comments
	local valid_text = vim.tbl_filter(function(line_pair)
		local line = line_pair[1]
		return not (vim.startswith(line, '"' .. opts.comment_character) or line == '""')
	end, tmux_text)

	local joined = table.concat(vim.tbl_flatten(valid_text), ' ')

	vim.fn.system 'tmux send-keys -t+ C-u' -- clear line
	vim.fn.system 'tmux send-keys -t+ ENTER' -- in the event we were searching, clear serach
	vim.fn.system 'tmux send-keys -t+ C-u' -- in the event we were searching, clear the line
	vim.fn.system('tmux send-keys -t+ ' .. joined)
end

local function function_surrounding_cursor()
	local current_node = vim.treesitter.get_node { bufnr = 0 }

	if not current_node then return '' end

	local func = current_node

	while func do
		-- if func:type() == 'function_declaration' then
		if func:type() == 'decl_def' then break end

		local parent = func:parent()
		func = parent
	end

	if not func then return '' end

	return vim.treesitter.get_node_text(func, 0)
end

function M.send_def_to_pane()
	local fn = function_surrounding_cursor()
	send_to_pane(fn)
end

function M.send_buffer_to_pane()
	local text = vim.api.nvim_buf_get_lines(0, 0, -1, false)
	send_to_pane(text)
end

function M.send_selection_to_pane()
	local text = U.get_visual_selection()
	send_to_pane(text)
end

--[[stylua: ignore]] --format
U.ft_localleader('nu', {
	{ 'ee', function() M.send_def_to_pane() end   , 'Evaluate def'    },
	{ 'eb', function() M.send_buffer_to_pane() end, 'Evaluate buffer' },
	{ 'e' , function() M.send_buffer_to_pane() end, 'Evaluate buffer', mode = 'v' },
})
