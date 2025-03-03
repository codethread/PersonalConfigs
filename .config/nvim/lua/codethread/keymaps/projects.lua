-- Project based keymaps
local root = U.git_root()
local p = function(path) return root == vim.fn.expand(path) end

if p '~/PersonalConfigs' then
	Keys.list({ prefix = '<leader>p' }, {
		{ 'p', 'lint', function() print 'hey' end },
	})
elseif p '~/dev/projects/pomo' then
	print 'hello projec'
end
