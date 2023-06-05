local Job = require 'plenary.job'

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

if in_dotfiles and vim.fn.executable 'dotty' then
	---@type Job
	local dotty_setup_job = Job:new {
		command = 'dotty',
		args = { 'setup' },
		on_start = function() dotty_info 'Running' end,
		-- on_exit = function() dotty_info 'Done' end,
	}

	vim.api.nvim_create_user_command('Dotty', function() dotty_setup_job:start() end, {})

	vim.api.nvim_create_autocmd({ 'BufWritePost', 'BufFilePost', 'BufWipeout', 'VimLeavePre' }, {
		desc = 'Run dotty when saving and deleting buffers',
		group = vim.api.nvim_create_augroup('Dotty', {}),
		pattern = os.getenv 'DOTFILES' .. '/*',
		callback = function(opts)
			if opts.file == nil then return end

			local job = Job:new {
				command = 'dotty',
				args = { 'test', opts.file },
			}

			job:and_then_on_success(dotty_setup_job)

			job:start()
		end,
	})
end
