-- WIP
function _G.my_set_terminal_goto()
	-- winnr = winnr or 0
	-- local winnr = vim.api.nvim_get_current_win()
	-- local path = vim.api.nvim_win_get_cursor(winnr)
	-- print(vim.inspect(path))
	local path = vim.fn.expand("<cfile>")
	-- print(vim.fn.expand("<cfile>"))
	-- vim.api.nvim_open_win
	vim.api.nvim_command("wincmd p") -- go back to previous window

	-- vim.call("e", path)
	local exists = vim.fn.bufexists(path)
	print(exists)
	-- vim.cmd([[:e expand('<cfile>')]])
end

vim.cmd([[
    augroup myterminal
      autocmd!
      autocmd TermOpen * nnoremap <buffer> gx :lua my_set_terminal_goto()<CR>
    augroup END
]])
