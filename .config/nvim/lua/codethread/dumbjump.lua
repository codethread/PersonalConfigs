local M = {}

function M.jump()
	local word = vim.fn.expand '<cword>'

	-- NOTE: nushell specific right now
	local search_term = '(def|local) ' .. word
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

	-- .config/nu/ct/tmux/projects.nu:1:8:export def get-projects [] {
	if #lines == 1 then
		local parts = vim.split(lines[1], ':')
		if #parts == 2 then
			vim.cmd.e(parts[1])
		elseif #parts == 3 then
			vim.cmd.e(parts[1] .. '|:' .. parts[2])
		elseif #parts == 4 then
			vim.cmd.e(parts[1])
			local keyword = vim.split(parts[4], ' ')[2]
			vim.api.nvim_win_set_cursor(0, { tonumber(parts[2]), tonumber(parts[3]) + #keyword })
		end
	end
	vim.cmd [[norm zz]]
end

return M
