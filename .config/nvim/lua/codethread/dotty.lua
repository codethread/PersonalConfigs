local Job = require 'plenary.job'

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

local dotfiles = '*/PersonalConfigs/*'
local group = vim.api.nvim_create_augroup('Dotty', { clear = false })

if vim.fn.executable 'dotty' then
	vim.api.nvim_clear_autocmds { group = 'Dotty' }

	-- run dotty when creating and deleting buffers
	vim.api.nvim_create_autocmd({ 'BufWritePost', 'BufFilePost', 'BufWipeout', 'VimLeavePre' }, {
		group = group,
		pattern = dotfiles,
		callback = function(opts)
			if opts.file == nil then return end

			Job:new({
				command = 'dotty',
				args = { 'test', opts.file },
				and_then_on_success = dotty_setup_job,
			}):start()
		end,
	})

	-- handle nvim-tree which has it's own api
	vim.api.nvim_create_autocmd('VimEnter', {
		group = group,
		pattern = dotfiles,
		callback = function()
			require('codethread.utils').safe_load('nvim-tree.api', function(api)
				local events = { 'NodeRenamed', 'FileCreated', 'FileRemoved', 'FolderRemoved' }
				for _, event in ipairs(events) do
					api.events.subscribe(event, function() dotty_setup_job:start() end)
				end
			end)
		end,
	})
else
	vim.notify('No Dotty bin, you may need to brew install', 'warn', {
		title = 'Dotty',
	})
end
