local api = vim.api
local M = {}

---Split the current window and open the previous buffer in left split
---handy when retrospectively wishing the file had opened in a split, e.g go-to-def
---
---For the future, I may want this to always use an existing pane, like a poor man's
---popout window
function M.split()
	local ow = api.nvim_get_current_win()
	vim.cmd.vsplit()
	local nw = api.nvim_get_current_win()
	vim.fn.win_gotoid(ow)
	vim.cmd 'b#'
	vim.fn.win_gotoid(nw)
end

return M
