local M = {}

-- Global state for recording and transcription
_G.claude_recording_active = false
_G.claude_transcription_active = false
_G.claude_recording_process = nil

function M.claude_chat()
	-- Create floating window with Snacks.win
	local win = require 'snacks.win' {
		title = 'Claude Chat',
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
			['<C-r>'] = function(self)
				-- Toggle recording: start if not active, stop if active
				if _G.claude_recording_active then
					-- Stop recording by sending SIGUSR1
					_G.claude_recording_active = false
					_G.claude_transcription_active = true
					vim.cmd('redrawstatus')

					vim.system({ 'audio--stop-recording' }, {
						text = true,
					}, function(result)
						vim.schedule(function()
							if result.code ~= 0 then
								local error_msg = result.stderr or 'Failed to stop recording'
								vim.notify('Error: ' .. error_msg, vim.log.levels.ERROR)
								_G.claude_transcription_active = false
								vim.cmd('redrawstatus')
							end
						end)
					end)
				else
					-- Start recording
					_G.claude_recording_active = true
					vim.cmd('redrawstatus')

					-- Run audio--record-and-transcribe in immediate mode
					_G.claude_recording_process = vim.system({ 'audio--record-and-transcribe', '--immediate' }, {
						text = true,
					}, function(result)
						vim.schedule(function()
							-- Clear flags
							_G.claude_recording_active = false
							_G.claude_transcription_active = false
							_G.claude_recording_process = nil
							vim.cmd('redrawstatus')

							if result.code == 0 and result.stdout then
								-- Trim whitespace from transcription
								local text = result.stdout:gsub('^%s*(.-)%s*$', '%1')

								if text ~= '' then
									-- Get current cursor position
									local cursor = vim.api.nvim_win_get_cursor(self.win)
									local row = cursor[1] - 1

									-- Get current line and insert transcription
									local current_line = vim.api.nvim_buf_get_lines(self.buf, row, row + 1, false)[1] or ''

									-- If current line is empty, insert transcription directly
									-- Otherwise, insert on next line
									if current_line == '' then
										vim.api.nvim_buf_set_lines(self.buf, row, row + 1, false, { text })
									else
										vim.api.nvim_buf_set_lines(self.buf, row + 1, row + 1, false, { '', text })
										-- Move cursor to the new line
										vim.api.nvim_win_set_cursor(self.win, { row + 2, 0 })
									end

									vim.notify('Transcription inserted', vim.log.levels.INFO)
								else
									vim.notify('No transcription output', vim.log.levels.WARN)
								end
							else
								local error_msg = result.stderr or 'Transcription failed'
								vim.notify('Error: ' .. error_msg, vim.log.levels.ERROR)
							end
						end)
					end)

					vim.notify('🎤 Recording... Press C-r again to stop', vim.log.levels.INFO)
				end
			end,
			['<C-x>'] = function(self)
				-- Cancel recording if active
				if _G.claude_recording_active or _G.claude_transcription_active then
					-- Kill the recording process
					if _G.claude_recording_process then
						_G.claude_recording_process:kill(9)
					end

					-- Clear all flags
					_G.claude_recording_active = false
					_G.claude_transcription_active = false
					_G.claude_recording_process = nil
					vim.cmd('redrawstatus')

					vim.notify('Recording cancelled', vim.log.levels.WARN)
				end
			end,
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

	-- Set buffer-local winbar directly to bypass lualine formatting/padding
	vim.api.nvim_buf_set_option(win.buf, 'filetype', 'markdown')
	vim.api.nvim_win_set_option(win.win, 'winbar', 'C-s:send | C-r:record | C-x:cancel | q:quit')
end

return M
