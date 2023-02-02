local Job = require 'plenary.job'

local heuristics = {
	['lua'] = {
		implementation = '.lua',
		test = '_spec.lua',
	},
}

-- local alt_map = {
-- 	['lua'] = {
-- 		implementation = 'test',
-- 		test = 'implementation',
-- 	},
-- }

local M = {}

-- get the alternate file based on heuristic
function M.alt_file()
	local file = vim.fn.expand '%:p'
	-- get the current file relative to the cwd
	local file_relative = vim.fn.fnamemodify(file, ':~:.')
	-- get the current filetype
	local filetype = vim.bo.filetype
	-- get the current filename
	local filename_only = vim.fn.expand '%:t'

	-- get the heuristic for the current filetype
	local heuristic = heuristics[filetype]
	if heuristic == nil then
		print('No heuristic for filetype: ' .. filetype)
		return
	end

	-- get most specific heuristic
	local best_heuristic = nil
	local filename = ''
	if vim.endswith(file_relative, heuristic.test) then
		best_heuristic = 'implementation'
		filename = string.gsub(filename_only, heuristic.test, '')
	else
		if vim.endswith(file_relative, heuristic.implementation) then
			best_heuristic = 'test'
			filename = string.gsub(filename_only, heuristic.implementation, '')
		end
	end

	if best_heuristic == nil then
		print('No heuristic match for filetype: ' .. filetype)
		return
	end

	Job:new({
		command = 'fd',
		args = { '-t', 'f', filename .. heuristic[best_heuristic] },
		on_exit = function(r, code)
			if code ~= 0 then
				print 'Error running fd'
				return
			end
			-- open file in new buffer
			vim.schedule(function() vim.cmd('edit ' .. r:result()[1]) end)
		end,
	}):start()
end

vim.api.nvim_create_user_command(
	'A',
	function() M.alt_file() end,
	{ desc = 'Jump to Alternate file' }
)

return M
