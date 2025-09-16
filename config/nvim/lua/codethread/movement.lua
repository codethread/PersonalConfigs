local hydra_ok, Hydra = pcall(require, 'hydra')
if not hydra_ok then
	print 'could not load hydra'
	return
end

local mover_hint = [[
 Params:
 move: ← _h_ → _l_
 goto: ↓ _j_ ↑ _k_
]]

local mover_hydra = Hydra {
	name = 'Param Mover',
	mode = 'n',
	hint = mover_hint,
	config = {
		invoke_on_body = true,
	},
	heads = {
		{ 'j', Cmd 'TSTextobjectGotoNextStart @parameter.inner' },
		{ 'k', Cmd 'TSTextobjectGotoPreviousStart @parameter.inner' },
		{ 'h', Cmd 'TSTextobjectSwapPrevious @parameter.inner' },
		{ 'l', Cmd 'TSTextobjectSwapNext @parameter.inner' },
	},
}

local tab_hydra = Hydra {
	name = 'Tab Jumper',
	mode = 'n',
	hint = [[
 Params:
 move: ← _h_ → _l_
 exit: _j_
 close: _x_
 new: _n_
]],

	config = {
		invoke_on_body = true,
		hint = {
			position = 'top-left',
			offset = 1,
		},
	},
	heads = {
		{ 'j', nil, { exit = true } },
		{ 'x', Cmd 'tabclose' },
		{ 'h', 'gT' },
		{ 'l', 'gt' },
		{ 'n', Cmd 'tabnew', { exit = true } },
	},
}

-- TODO: some more cool plugins:
-- https://github.com/mfussenegger/nvim-treehopper

return {
	mover_hydra = mover_hydra,
	tab_hydra = tab_hydra,
}
