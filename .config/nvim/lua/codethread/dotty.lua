local M = {}

local Job = require 'plenary.job'

-- TODO can expose this from dotty
local in_dotfiles =
	vim.endswith(vim.fn.getcwd(), os.getenv 'DOTFILES' or os.getenv 'HOME' .. '/.config')

---notify at info level
---@param msg string
local function dotty_info(msg)
	vim.notify(msg, vim.log.levels.INFO, {
		title = 'Dotty',
		hide_from_history = true,
		timeout = 0,
	})
end

M.dotty_link = Job:new {
	command = 'nush',
	args = { [[use ct/dotty; dotty link]] },
	on_start = function() dotty_info 'Running' end,
	on_exit = function(j, code)
		if code ~= 0 then
			vim.notify(j:stderr_result(), vim.log.levels.ERROR, {
				title = 'dotty',
				timeout = 3000,
			})
		end
	end,
}

if in_dotfiles then
	local dotty_test = function(file)
		return Job:new {
			command = 'nush',
			args = { [[use ct/dotty; dotty test ]] .. file },
		}
	end

	vim.api.nvim_create_user_command('Dotty', function() M.dotty_link:start() end, {})

	vim.api.nvim_create_user_command(
		'DottyTest',
		function() dotty_test(vim.fn.bufname()):start() end,
		{}
	)

	vim.api.nvim_create_autocmd({ 'BufWritePost', 'BufFilePost', 'VimLeavePre' }, {
		desc = 'Run dotty when saving and deleting buffers',
		group = vim.api.nvim_create_augroup('Dotty', {}),
		callback = function(opts)
			if opts.file == nil then return end
			M.dotty_link:start()

			-- local j = dotty_test(opts.file)
			-- j:and_then_on_success(dotty_link)
			-- j:start()
		end,
	})
end

return M
