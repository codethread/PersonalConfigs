--   ╭─────────────────────────────────────────────────────────────────────────╮
--   │                          Draw nice ascii boxes                          │
--   ╰─────────────────────────────────────────────────────────────────────────╯
local M = {}

M.width = 80
M.padding = 2

local box = {
	horz = '─',
	vert = '│',
	tl = '╭',
	tr = '╮',
	bl = '╰',
	br = '╯',
}

local function compose(msg, comment)
	local span = M.width - 2 - #comment
	local text_padding_width = math.floor((span - #msg) / 2)
	local text_padding = string.rep(' ', text_padding_width)
	local remainder = span - #(text_padding .. msg .. text_padding)
	local left_pad = string.rep(' ', M.padding)
	return {
		string.format(comment, left_pad .. box.tl .. string.rep(box.horz, span) .. box.tr),
		string.format(
			comment,
			left_pad
				.. box.vert
				.. text_padding
				.. msg
				.. text_padding
				.. string.rep(' ', remainder)
				.. box.vert
		),
		string.format(comment, left_pad .. box.bl .. string.rep(box.horz, span) .. box.br),
	}
end

function M.box()
	local b = vim.api.nvim_get_current_buf()
	local cur = vim.api.nvim_win_get_cursor(0)
	local row = cur[1]
	local comment = vim.bo.commentstring

	if not comment then
		vim.notify('no commentstring sest', vim.log.levels.WARN)
		return
	end

	vim.ui.input({ prompt = 'Comment' }, function(value)
		if not value then return end
		vim.api.nvim_buf_set_lines(b, row, row, false, compose(value, comment))
	end)
end

return M
