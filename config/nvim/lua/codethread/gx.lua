-- Extends Neovim's link handling with custom behavior for various file and URL types
local M = {}

M.gx = function()
	local curFile = vim.fn.expand '%:t'
	local file = vim.fn.expand '<cfile>'

	if vim.startswith(file, 'http') then
		vim.ui.open(file)
	elseif vim.startswith(file, 'file://') then
		-- NOTE: currently only designed for jumping in a markdown hover
		--
		-- can have syntax file:///foo/bar#L1 in the case of markdown in lsp hover
		local _, _, path, line = file:find 'file://(.*)#L(.*)'
		if not path then error('regex needs changing, likely no line number: ' .. file) end
		vim.cmd.norm"q" -- close hover
		vim.cmd(string.format('keepjumps view +%s %s', line, path))
	elseif curFile == 'package.json' then
		M.open_npm_package()
	elseif vim.bo.ft == 'lua' and file:find '^([%w-_]+/[%w-_.]+)$' then -- github link like foo/bar.nvim
		vim.ui.open('https://github.com/' .. file)
	elseif vim.system({ 'isPhrase', file }):wait().code == 0 then
		M.open_phrase_key(file)
	else
		print('not a link: ' .. file)
	end
end

---Open picker for npm package under cursor
---@package
function M.open_npm_package()
	local pkg, version = vim.api.nvim_get_current_line():match '"(.*)": "(.*)"'
	vim.ui.open(string.format('https://www.npmjs.com/package/%s/v/%s', pkg, version))
end

---comment
---@param key string
---@package
function M.open_phrase_key(key)
	local file_path = U.get_current_file()
	local dir = vim.split(file_path, '/')[2]
	local dirs = { 'native', 'web', 'clientShared' }

	if not vim.list_contains(dirs, dir) then
		local msg = string.format(
			'%s is not an expected dir of %s in path %s',
			dir,
			table.concat(dirs, ','),
			file_path
		)
		error(msg)
	end

	local web = 'apps/web/app/public/locale/web/en-gb'
	local native = 'apps/web/app/public/locale/app/en-gb'

	---@type string[]
	local serach_dirs = dir == 'web' and { web } or dir == 'native' and { native } or { web, native }

	local cmd = {
		'rg',
		key,
		'-F',
		'--vimgrep',
		unpack(serach_dirs),
	}

	vim.system(cmd, { text = true }, function(obj)
		local lines = vim.split(vim.trim(obj.stdout), '\n')
		---@type string
		local target
		if #lines == 1 then
			target = vim.split(lines[1], ':  ')[1]
			vim.system { 'openInVim', target }
		else
			vim.ui.select(lines, {
				prompt = 'Phrase file',
				format_item = function(line)
					local path, match = unpack(vim.split(line, '  ', { plain = true }))
					return path:gsub('apps/web/app/public/locale/', '') .. ' | ' .. match
				end,
			}, function(choice)
				if choice then
					target = vim.split(choice, ':  ')[1]
					vim.system { 'openInVim', target }
				end
			end)
		end
	end)
end

return M
