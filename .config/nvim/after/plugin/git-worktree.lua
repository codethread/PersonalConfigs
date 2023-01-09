local U = require 'codethread.utils'
local wt = require 'git-worktree'

wt.setup {}

wt.on_tree_change(function(op, meta)
	if op == wt.Operations.Switch then
		local prev = meta.prev_path
		local to = meta.path

		local old_s = prev .. '/Session.vim'
		local new_s = to .. '/Session.vim'

		if U.file_exits(old_s) then vim.cmd([[Obsess! ]] .. old_s) end

		if U.file_exits(new_s) then
			vim.cmd([[source ]] .. new_s)
		else
			vim.cmd([[Obsess ]] .. new_s)
		end
	end
end)
