local M = {}

---Open the next file in the directory tree (alphabetically).
---Useful in the context of running a macro over files in a directory
function M.open_next_file()
	local found = false
	local iter = vim.iter(vim.fs.dir(vim.fn.expand '%:h'))

	local next = iter:find(function(name, type)
		local current = vim.fn.expand '%:t'
		if found and type == 'file' then
			found = false
			return true
		elseif type == 'file' and name == current then
			found = true
		end
	end)
	if not next then
		error('no more files', 0)
	else
		local to = vim.fn.expand '%:h' .. '/' .. next
		vim.cmd.e(to)
	end
end

vim.api.nvim_create_user_command(
	'OpenNextFile',
	M.open_next_file,
	{ desc = 'Open next file in directory' }
)

---Save the current buffer, but also handles oil buffers
function M.save_buffer()
	local ft = vim.bo.filetype
	if ft == 'oil' then
		require('oil').save(nil, function(err)
			if err then
				vim.notify(err, vim.log.levels.ERROR, { title = 'Oil' })
			else
				require('codethread.dotty').dotty_link()

				local path = string.gsub(vim.fn.expand '%', 'oil://', '')

				require('plugins.notes.backup').update_and_push('renames', path)
			end
		end)
	else
		vim.cmd.w()
	end
end

function M.test_current_file()
	local ft = vim.bo.filetype
	if ft == 'lua' then
		vim.cmd 'w'
		-- require('plenary.test_harness').test_directory(vim.fn.expand '%:p')
		require('plenary.test_harness').test_directory(
			vim.fn.expand '%:p',
			-- if writing lua tests, I'll follow the same setup as https://github.com/m00qek/plugin-template.nvim
			{ minimal_init = 'test/spec.vim' }
		)
		-- also PlenaryTestFile
	elseif ft == 'javascript' then
		local jest = require 'jester'
		jest.run_last { path_to_jest = './node_modules/bin/jest' }
	elseif ft == 'go' then
		-- vim.Cmd.GoTestFunc()
		vim.cmd.GoTestSubCase()
	else
		print('no setup for filetype: ' .. ft)
	end
end

function M.toggle_file_history()
	local is_diff = vim.startswith(vim.fn.expand '%', 'diffview://')
	if is_diff then
		vim.cmd [[DiffviewClose]]
	else
		vim.cmd [[DiffviewFileHistory % --no-merges]]
	end
end

function M.align_by(char)
	local txt = U.get_visual_selection()
	local lines = vim.split(txt, '\n', { trimempty = true })
	for _, line in ipairs(lines) do
		print(line)
	end
end

vim.api.nvim_create_user_command('Testy', M.align_by, {})

function M.store_to_clipboard(str)
	if not type(str) == 'string' then
		vim.notify 'should be string!'
		return
	end
	vim.cmd([[let @*="]] .. str .. '"')
end

function M.copy_filepath_relative() local p = vim.fn.expand '%:p' end

function M.debounce(ms, fn)
	local timer = vim.uv.new_timer()
	return function(...)
		local argv = { ... }
		timer:start(ms, 0, function()
			timer:stop()
			vim.schedule_wrap(fn)(unpack(argv))
		end)
	end
end

---comment
---@param key string
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
