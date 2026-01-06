local api = vim.api
local M = {}

---Split the current window and open the previous buffer in the new split
---handy when retrospectively wishing the file had opened in a split, e.g go-to-def
---@param direction? 'up' | 'left' | 'right' | 'down' | 'tab'
function M.split(direction)
	local ow = api.nvim_get_current_win()
	local is_split_down = vim.opt.splitbelow:get()
	local is_split_right = vim.opt.splitright:get()

	if direction == 'tab' then
		vim.cmd 'tab split'
		vim.cmd 'b#'
		return
	end

	if direction == 'up' then
		if is_split_down then
			vim.opt.splitbelow = false
			vim.cmd.split()
			vim.opt.splitbelow = is_split_down
		else
			vim.cmd.split()
		end
	elseif direction == 'down' then
		if not is_split_down then
			vim.opt.splitbelow = true
			vim.cmd.split()
			vim.opt.splitbelow = is_split_down
		else
			vim.cmd.split()
		end
	elseif direction == 'left' then
		if is_split_right then
			vim.opt.splitright = false
			vim.cmd.vsplit()
			vim.opt.splitright = is_split_right
		else
			vim.cmd.vsplit()
		end
	else -- 'right' or nil (default)
		if not is_split_right then
			vim.opt.splitright = true
			vim.cmd.vsplit()
			vim.opt.splitright = is_split_right
		else
			vim.cmd.vsplit()
		end
	end

	local nw = api.nvim_get_current_win()
	api.nvim_set_current_win(ow)
	vim.cmd 'b#'
	api.nvim_set_current_win(nw)
end

return M
