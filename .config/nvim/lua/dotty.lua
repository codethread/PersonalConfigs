local Job = require 'plenary.job'

local in_dotfiles = vim.endswith(vim.fn.getcwd(), 'PersonalConfigs')

---notify at info level
---@param msg string
local function dotty_info(msg)
	vim.notify(msg, 'info', {
		title = 'Dotty',
		hide_from_history = true,
		timeout = 0,
	})
end

local dotty_setup_job = Job:new {
	command = 'dotty',
	args = { 'setup' },
	on_start = function() dotty_info 'Running' end,
}

vim.api.nvim_create_user_command('Dotty', function() dotty_setup_job:start() end, {})

if in_dotfiles then
	if vim.fn.executable 'dotty' then
		vim.api.nvim_create_autocmd({ 'BufWritePost', 'BufFilePost', 'BufWipeout', 'VimLeavePre' }, {
			desc = 'Run dotty when saving and deleting buffers',
			group = vim.api.nvim_create_augroup('Dotty', {}),
			pattern = '*/PersonalConfigs/*',
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

		-- handle nvim-tree which has it's own api
		require('codethread.utils').safe_load('nvim-tree.api', function(api)
			local events = { 'NodeRenamed', 'FileCreated', 'FileRemoved', 'FolderRemoved' }
			for _, event in ipairs(events) do
				api.events.subscribe(event, function() dotty_setup_job:start() end)
			end
		end)
	else
		vim.notify('No Dotty bin, you may need to brew install', 'warn', {
			title = 'Dotty',
		})
	end
end
