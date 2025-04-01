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

---Save the current buffer, but also handles oil buffers
function M.save_buffer()
	local ft = vim.bo.filetype
	if ft == 'oil' then
		local ok, oil = pcall(require, 'oil')
		assert(ok, 'no oil!')
		oil.save(nil, function(err)
			if err then
				vim.notify(err, vim.log.levels.ERROR, { title = 'Oil' })
			else
				require('codethread.dotty').dotty_link()
				local path = string.gsub(vim.fn.expand '%', 'oil://', '')
				require('plugins.notes.backup').update_and_push('renames', path)
			end
		end)

		-- too annoying as it alters jump list
		-- elseif vim.tbl_contains({ 'typescript', 'typescriptreact', 'javascript' }, ft) then
		-- 	require('vtsls').commands.remove_unused_imports(
		-- 		vim.api.nvim_get_current_buf(),
		-- 		function() vim.cmd.w() end,
		-- 		function(e)
		-- 			dd(e)
		-- 			vim.cmd.w()
		-- 		end
		-- 	)
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

---Show listchars to see things like tabs/spaces and trailing lines
function M.toggle_listchars()
	local state = vim.b.ct_debug_spacing or false
	if state then
		vim.b.ct_debug_spacing = false
		vim.b.miniindentscope_disable = false
		vim.cmd 'set nolist'
		vim.api.nvim_set_hl(0, 'Whitespace', { fg = U.hl 'base' })
		require('ibl').setup_buffer(0, { enabled = true })
	else
		vim.b.ct_debug_spacing = true
		vim.b.miniindentscope_disable = true
		vim.cmd 'set list'
		vim.api.nvim_set_hl(0, 'Whitespace', { fg = U.hl 'leaf' })
		require('ibl').setup_buffer(0, { enabled = false })
	end
end

function M.toggle_diagnostics()
	local state = vim.b.ct_diagnostics_off or true
	if state then
		vim.diagnostic.hide()
		vim.b.ct_diagnostics_off = true
	else
		vim.diagnostic.show()
		vim.b.ct_diagnostics_off = false
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

function M.toggle_indent_scope()
	local state = vim.b.ct_indent_scope or 'scope'
	if state == 'indent' then
		require('ibl').setup_buffer(0, {
			scope = { enabled = true },
		})

		vim.b.miniindentscope_disable = true
		vim.b.ct_indent_scope = 'scope'
	else
		require('ibl').setup_buffer(0, {
			scope = { enabled = false },
		})
		vim.b.miniindentscope_disable = false
		vim.b.ct_indent_scope = 'indent'
	end
end

function M.worktree_open_alt()
	U.nush(
		[[git worktree list | parse '{p} {sha} {branch}' | str trim | get p | to text]],
		{},
		vim.schedule_wrap(function(out)
			local wks = vim.split(out.stdout, '\n')
			local str = require 'lib.str'
			local full_path = vim.fn.expand '%:p'
			local buffer_path = vim.fn.expand '%'
			assert(buffer_path ~= full_path, 'paths should not be equal') -- sometimes this happens and not sure why
			local wktrs = vim
				.iter(wks)
				:filter(function(wk) return not str(full_path):includes(wk) end)
				:totable()
			vim.ui.select(
				wktrs,
				{},
				function(choice) vim.cmd.vsplit(vim.fs.joinpath(choice, buffer_path)) end
			)
		end)
	)
end

function M.reload_snippets()
	require('luasnip.loaders.from_vscode').load {
		paths = {
			'~/.config/nvim/snippets_vscode',
			'~/.local/share/nvim/lazy/friendly-snippets',
		},
	}
end

function M.git_reset_buffer()
	require('gitsigns').reset_buffer()
	vim.cmd [[noa w]]
end

function M.toggle_quickfix()
	for _, win in ipairs(vim.api.nvim_tabpage_list_wins(vim.api.nvim_get_current_tabpage())) do
		if vim.fn.getwinvar(win, '&syntax') == 'qf' then
			vim.cmd.cclose()
			return
		end
	end
	vim.cmd.copen()
end

return M
