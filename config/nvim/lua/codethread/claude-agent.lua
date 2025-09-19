local M = {}

-- Parse YAML frontmatter from markdown content
local function parse_frontmatter(content)
	local lines = vim.split(content, '\n')
	local in_frontmatter = false
	local frontmatter_lines = {}
	local content_lines = {}

	for i, line in ipairs(lines) do
		if i == 1 and line == '---' then
			in_frontmatter = true
		elseif in_frontmatter and line == '---' then
			in_frontmatter = false
		elseif in_frontmatter then
			table.insert(frontmatter_lines, line)
		else
			if #frontmatter_lines > 0 or line ~= '---' then table.insert(content_lines, line) end
		end
	end

	return table.concat(frontmatter_lines, '\n'), table.concat(content_lines, '\n')
end

-- Expand escape sequences in YAML values
local function expand_yaml_escapes(yaml_text)
	local lines = vim.split(yaml_text, '\n')
	local result = {}

	for _, line in ipairs(lines) do
		-- Check if line contains a YAML key-value pair
		local key, value = line:match '^(%s*[%w_]+:%s*)(.*)'
		if key and value ~= '' then
			-- Detect the escaping pattern used and normalize to single escapes first
			-- Replace any number of backslashes followed by n with just \n
			local expanded = value:gsub('\\+n', '\n')
			expanded = expanded:gsub('\\+t', '\t')
			expanded = expanded:gsub('\\+r', '\r')
			expanded = expanded:gsub('\\+"', '"')
			expanded = expanded:gsub("\\+'", "'")

			-- For multiline values, indent continuation lines
			local expanded_lines = vim.split(expanded, '\n')
			if #expanded_lines > 1 then
				table.insert(result, key .. expanded_lines[1])
				for i = 2, #expanded_lines do
					table.insert(result, '  ' .. expanded_lines[i])
				end
			else
				table.insert(result, key .. expanded)
			end
		else
			table.insert(result, line)
		end
	end

	return table.concat(result, '\n')
end

-- Minify YAML content by collapsing multiline values
local function minify_yaml(yaml_text)
	local lines = vim.split(yaml_text, '\n')
	local result = {}
	local current_key = nil
	local current_value = {}

	local function flush_current()
		if current_key then
			-- Join multi-line values with \\n (double backslash)
			local value = table.concat(current_value, '\\n')
			-- Only escape actual newlines/tabs/etc that appear in the text
			value = value:gsub('\n', '\\n'):gsub('\t', '\\t'):gsub('\r', '\\r')

			table.insert(result, current_key .. value)
			current_key = nil
			current_value = {}
		end
	end

	for _, line in ipairs(lines) do
		-- Check if this is a new key-value pair
		local key, value = line:match '^([%w_]+:%s*)(.*)'
		if key then
			flush_current()
			current_key = key
			if value ~= '' then table.insert(current_value, value) end
		elseif line:match '^%s+' and current_key then
			-- Continuation of previous value
			local content = line:match '^%s+(.*)'
			table.insert(current_value, content or '')
		else
			flush_current()
			if line ~= '' then table.insert(result, line) end
		end
	end

	flush_current()
	return table.concat(result, '\n')
end

-- Edit a Claude agent file focussing on the yaml which will be expanded and then collapsed on save
function M.edit_agent_file(filepath)
	if not filepath then filepath = vim.fn.expand '%:p' end

	-- Get the buffer content instead of reading from disk
	local content
	local bufnr = vim.fn.bufnr(filepath)
	if bufnr == -1 or not vim.api.nvim_buf_is_loaded(bufnr) then
		-- File not open in a buffer, fall back to reading from disk
		local file = io.open(filepath, 'r')
		if not file then
			vim.notify('Could not open file: ' .. filepath, vim.log.levels.ERROR)
			return
		end
		content = file:read '*a'
		file:close()
	else
		-- Use buffer content
		local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
		content = table.concat(lines, '\n')
	end

	-- Parse frontmatter
	local frontmatter, body = parse_frontmatter(content)
	if frontmatter == '' then
		vim.notify('No YAML frontmatter found in file', vim.log.levels.WARN)
		return
	end

	-- Expand escape sequences for editing
	local expanded = expand_yaml_escapes(frontmatter)

	-- Create floating window with Snacks.win
	local win = require 'snacks.win' {
		title = 'Edit Agent Frontmatter: '
			.. vim.fn.fnamemodify(filepath, ':t')
			.. ' (q:quit, C-s:save)',
		text = expanded,
		width = 0.7,
		height = 0.8,
		position = 'float',
		border = 'rounded',
		ft = 'yaml',
		wo = {
			wrap = true,
			linebreak = true,
			number = true,
			relativenumber = true,
		},
		keys = {
			q = function(self) self:close() end,
			['<C-s>'] = function(self)
				-- Get the buffer content
				local buf_lines = vim.api.nvim_buf_get_lines(self.buf, 0, -1, false)
				local new_frontmatter = table.concat(buf_lines, '\n')

				-- Minify the YAML content
				local minified = minify_yaml(new_frontmatter)

				-- Reconstruct the file with updated frontmatter
				local new_content = '---\n' .. minified .. '\n---\n' .. body

				-- Update the buffer if it's open, otherwise write to file directly
				local bufnr = vim.fn.bufnr(filepath)
				if bufnr ~= -1 and vim.api.nvim_buf_is_loaded(bufnr) then
					-- Split content into lines and update buffer
					local new_lines = vim.split(new_content, '\n')
					vim.api.nvim_buf_set_lines(bufnr, 0, -1, false, new_lines)

					-- Save the buffer
					vim.api.nvim_buf_call(bufnr, function() vim.cmd 'write!' end)

					vim.notify('Agent file updated: ' .. filepath, vim.log.levels.INFO)
					self:close()
				else
					-- No buffer open, write directly to file
					local out_file = io.open(filepath, 'w')
					if out_file then
						out_file:write(new_content)
						out_file:close()
						vim.notify('Agent file updated: ' .. filepath, vim.log.levels.INFO)
						self:close()
					else
						vim.notify('Could not write to file: ' .. filepath, vim.log.levels.ERROR)
					end
				end
			end,
		},
	}
end

return M
