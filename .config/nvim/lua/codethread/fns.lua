local M = {}

--TODO: make buffer local
local diagnostics_active = true
function M.toggle_diagnostic()
	diagnostics_active = not diagnostics_active
	if diagnostics_active then
		vim.diagnostic.show()
	else
		vim.diagnostic.hide()
	end
end

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
	local ft = U.ft()
	if ft == 'oil' then
		require('oil').save(nil, function(err)
			if err then
				vim.notify(err, vim.log.levels.ERROR, { title = 'Dotty' })
			else
				require('codethread.dotty').dotty_link()
			end
		end)
	else
		vim.cmd.w()
	end
end

function M.test_current_file()
	local ft = U.ft()
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

---Creates a table of contents for obsidian files
function M.create_markdown_toc()
	-- set a mark
	-- move to the top of the file and search for the end of the frontmatter
	vim.cmd [[silent execute "normal mzgg/---/e+1\<CR>n"]]
	local needs_creating = vim.fn.search('vim-markdown-toc', 'wn') == 0
	if needs_creating then
		vim.cmd [[silent GenTocGFM]]
	else
		vim.cmd [[silent UpdateToc]]
	end
	-- go back to the mark and center the buffer
	vim.cmd [[silent execute "norm 'zzz"]]
end

return M
