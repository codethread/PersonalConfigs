local pattern = '/Users/codethread/Library/Mobile Documents/iCloud~md~obsidian/Documents/Notes/*'

local function group(name)
	vim.api.nvim_create_augroup('codethread_obsidian_' .. name, { clear = true })
end

local opened = vim.api.nvim_create_autocmd({ 'BufReadPre' }, {
	group = group 'git',
	pattern = pattern,
	callback = function() end,
})
