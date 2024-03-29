local M = {}

function M.jump()
	local word = vim.fn.expand '<cword>'

	-- NOTE: nushell specific right now
	local search_term = '(def|let) ' .. word
	local glob = '.config/nu/**/*.nu'

	-- so fast sync is fine
	local stdout =
		vim.system({ 'rg', search_term, '--hidden', '--vimgrep', '--glob', glob }, { text = true })
			:wait().stdout

	if not stdout or stdout == '' then
		vim.notify('no definition found', vim.log.levels.ERROR)
		return
	end

	local lines = vim.split(vim.trim(stdout), '\n', {})

	if #lines == 1 then
		local path, lnum, _col, match = unpack(vim.split(lines[1], ':'))

		if not match or match == '' then
			vim.notify('result not vimgrep format', vim.log.levels.ERROR)
			vim.print(lines[1])
			return
		end

		local col = string.find(match, word, 1, true) - 1
		if not col then error('could not find def in match: ' .. match .. ', word: ' .. word) end
		vim.cmd.e(path)
		vim.api.nvim_win_set_cursor(0, { tonumber(lnum), tonumber(col) })
		vim.cmd [[norm zz]]
	end
end

function M.reload() require('plenary.reload').reload_module 'codethread.dumbjump' end

return M
