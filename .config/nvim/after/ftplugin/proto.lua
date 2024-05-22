vim.opt_local.iskeyword:append { '.' }

--- protobuf defs are namespaced like foo.bar.baz.Message
--- here we split out the . separated domain from the message
--- e.g foo.bar.baz and Message
local function to_parts(search)
	local parts = vim.split(search, '.', { plain = true })

	local word
	local domain

	for index, value in ipairs(parts) do
		if index == #parts then
			word = value
		else
			if domain then
				domain = domain .. '.' .. value
			else
				domain = value
			end
		end
	end

	return word, domain
end

local function get_first_def(stdout)
	if not stdout or stdout == '' then
		vim.notify('no definition found', vim.log.levels.ERROR)
		return
	end

	local lines = vim.split(vim.trim(stdout), '\n', {})

	if #lines ~= 1 then return end

	return lines[1]
end

local function dumpjump()
	local search = vim.fn.expand '<cword>'
	local glob = '*.proto'

	local word, package_name = to_parts(search)

	local stdout
	local same_file = false

	-- then this isn't namespaced, and therefore a local definition in this file
	if not package_name then
		same_file = true
		glob = vim.fn.expand '%'

		local search_term = '(message|enum) ' .. word .. ' \\{'
		local cmd = { 'rg', search_term, '--hidden', '--vimgrep', glob }
		stdout = vim.system(cmd, { text = true }):wait().stdout
	else
		local search_term = 'package ' .. package_name
		-- first we find the file
		stdout =
			vim.system({ 'rg', search_term, '--hidden', '-l', '--glob', glob }, { text = true })
				:wait().stdout

		local line = get_first_def(stdout)
		if not line then
			vim.notify('no def found for package ' .. package_name)
			return
		end

		-- search in file
		search_term = '(message|enum) ' .. word .. ' \\{'

		stdout = vim.system(
			{ 'rg', search_term, '--hidden', '--vimgrep', '--glob', line },
			{ text = true }
		)
			:wait().stdout
	end

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

		-- navigate to file
		if not same_file then vim.cmd.e(path) end

		vim.api.nvim_win_set_cursor(0, { tonumber(lnum), tonumber(col) })
		vim.cmd [[norm zz]]
	end
end

U.keys(0, {
	{ 'gd', dumpjump, 'Go to def' },
}, { prefix = '' })
