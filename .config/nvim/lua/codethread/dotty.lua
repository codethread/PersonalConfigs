local M = {}

local Job = require 'plenary.job'

local in_dotfiles = false

---notify at info level
---@param msg string
local function dotty_info(msg)
	-- remove return to show messages
	return nil
	-- vim.notify(msg, vim.log.levels.INFO, {
	-- 	title = 'Dotty',
	-- 	hide_from_history = true,
	-- 	timeout = 0,
	-- })
end

M.dotty_test = function(file)
	if not in_dotfiles then return nil end

	local j = Job:new {
		command = 'nush',
		args = { [[use ct/dotty; dotty test ]] .. file },
	}
	j:start()
	return j
end

M.dotty_link = function()
	if not in_dotfiles then return nil end

	local j = Job:new {
		command = 'nush',
		args = { [[use ct/dotty; dotty link --stdout]] },
		on_start = function() dotty_info 'Running' end,
		on_exit = function(j, code)
			if code ~= 0 then
				vim.notify(j:stderr_result(), vim.log.levels.ERROR, {
					title = 'dotty',
					timeout = 3000,
				})
			else
				dotty_info(j:result())
			end
		end,
	}
	j:start()
	return j
end

local function setup_autocmds()
	vim.api.nvim_create_user_command('Dotty', function() M.dotty_link() end, {})

	vim.api.nvim_create_user_command(
		'DottyTest',
		function() M.dotty_test(vim.fn.bufname()):start() end,
		{}
	)

	vim.api.nvim_create_autocmd({ 'BufWritePost', 'BufFilePost', 'VimLeavePre' }, {
		desc = 'Run dotty when saving and deleting buffers',
		pattern = vim.fn.getcwd() .. '/*',
		group = vim.api.nvim_create_augroup('Dotty', {}),
		callback = function(opts)
			if
				opts.file == nil
				or opts.match == nil
				or vim.startswith(opts.match:gsub(vim.fn.getcwd() .. '/', ''), 'Neogit')
			then
				return
			end

			M.dotty_link()
		end,
	})
end

if vim.fn.executable 'nush' == 1 then
	local root = vim.fs.root(0, '.git')
	if not root then return end

	Job:new({
		command = 'nush',
		args = { [[use ct/dotty; dotty is-cwd ]] .. root .. [[ --exit]] },
		on_exit = function(_, code)
			if code == 0 then
				in_dotfiles = true
				vim.schedule(setup_autocmds)
			end
		end,
	}):start()
else
	vim.notify('nush not present in PATH for dotty', vim.log.levels.WARN)
end

return M
