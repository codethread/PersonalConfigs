require('live-command').setup {
	defaults = {
		inline_highlighting = false,
	},
	break_undo = false,
	commands = {
		Norm = {
			-- :%Norm 0f{ciwlook mum, no hands!
			cmd = 'norm',
		},
		Reg = {
			-- This will transform ":5Reg a" into ":norm 5@a", running a captured macro with a preview
			cmd = 'norm',
			args = function(opts) return (opts.count == -1 and '' or opts.count) .. '@' .. opts.args end,
			range = '',
		},
		S = {
			-- run :%/result/outcome and see bottome text change, using vim-abolish
			cmd = 'Subvert',
		},
	},
}

-- result
-- Result
-- RESULT
