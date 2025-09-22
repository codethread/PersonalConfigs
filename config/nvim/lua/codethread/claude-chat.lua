local M = {}

function M.claude_chat()
	-- Create floating window with Snacks.win
	local win = require 'snacks.win' {
		title = 'Claude Chat (C-s:send, q:quit)',
		text = '',
		width = 0.8,
		height = 0.6,
		position = 'float',
		border = 'rounded',
		ft = 'markdown',
		wo = {
			wrap = true,
			linebreak = true,
			number = false,
			relativenumber = false,
			spell = true,
		},
		keys = {
			q = function(self) self:close() end,
			['<C-s>'] = function(self)
				-- Get the buffer content
				local buf_lines = vim.api.nvim_buf_get_lines(self.buf, 0, -1, false)
				local prompt = table.concat(buf_lines, '\n')

				-- Trim whitespace
				prompt = prompt:gsub('^%s*(.-)%s*$', '%1')

				if prompt == '' then
					vim.notify('No prompt to send', vim.log.levels.WARN)
					return
				end

				-- Send to Claude using the send-to-claude command
				local cmd = { 'send-to-claude', prompt }
				vim.system(cmd, {
					text = true,
				}, function(result)
					vim.schedule(function()
						if result.code == 0 then
							vim.notify('Prompt sent to Claude', vim.log.levels.INFO)
							self:close()
						else
							local error_msg = result.stderr or 'Failed to send prompt'
							vim.notify('Error: ' .. error_msg, vim.log.levels.ERROR)
						end
					end)
				end)
			end,
		},
	}

	-- Set initial content with a helpful comment
	vim.api.nvim_buf_set_lines(win.buf, 0, -1, false, {
		'',
	})
end

return M
