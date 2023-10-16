local M = {}

local function send_to_pane(text)
	local lines = type(text) == 'string' and vim.split(text, '\n', { plain = true }) or text

	local tmux_text = vim.tbl_map(function(line)
		-- escape charactes tmux things are special
		local escaped = string.gsub(line, '([%$%!%^%+%-"])', '\\%1')
		-- local escaped = line
		return { '"' .. escaped .. '"', '"ENTER"' }
	end, lines)

	local joined = table.concat(vim.tbl_flatten(tmux_text), ' ')

	vim.print(joined)
	vim.fn.system 'tmux send-keys -t+ C-u'
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

	return vim.treesitter.query.get_node_text(func, 0)
end

function M.send_def_to_pane()
	local fn = function_surrounding_cursor()
	send_to_pane(fn)
end

function M.send_buffer_to_pane()
	local text = vim.api.nvim_buf_get_lines(0, 0, -1, false)
	send_to_pane(text)
end

function _G.send_to_pane() M.send_def_to_pane() end

U.keys('nu', {
	{ 'ee', function() M.send_def_to_pane() end, 'Evaluate def' },
	{ 'ef', function() M.send_buffer_to_pane() end, 'Evaluate buffer' },
})
