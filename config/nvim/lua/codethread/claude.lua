local M = {}

function M.claude_query()
	vim.ui.input({ prompt = 'Claude query: ' }, function(prompt)
		if not prompt or prompt == '' then
			return
		end

		local current_file = vim.fn.expand '%:p'
		local cmd
		local opts = { text = true }

		if current_file ~= '' and vim.fn.filereadable(current_file) == 1 then
			-- Include filepath context and pipe file content to claude
			local full_prompt = string.format('File: %s\n\n%s', current_file, prompt)
			cmd = { 'sh', '-c', string.format('cat "%s" | claude -p "%s"', current_file, full_prompt:gsub('"', '\\"')) }
		else
			-- No file, just run claude with the prompt
			cmd = { 'claude', '-p', prompt }
		end

		vim.system(cmd, opts, function(obj)
			if obj.code == 0 then
				local lines = vim.split(vim.trim(obj.stdout), '\n')
				vim.schedule(function()
					vim.cmd 'new'
					vim.api.nvim_buf_set_lines(0, 0, -1, false, lines)
					vim.bo.buftype = 'nofile'
					vim.bo.bufhidden = 'wipe'
					vim.bo.swapfile = false
					vim.bo.filetype = 'markdown'
				end)
			else
				vim.schedule(function()
					vim.notify('Claude error: ' .. (obj.stderr or 'Unknown error'), vim.log.levels.ERROR)
				end)
			end
		end)
	end)
end

return M