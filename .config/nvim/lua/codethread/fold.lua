-- return the max fold level of the buffer (for now doing the opposite and folding incrementally is unbounded)
-- Also jarring if you start folding incrementally after opening all folds
local function max_level()
	-- return vim.wo.foldlevel -- find a way for this to return max fold level
	return 0
end

---Set the fold level to the provided value and store it locally to the buffer
---@param num integer the fold level to set
local function set_fold(num)
	-- vim.w.ufo_foldlevel = math.min(math.max(0, num), max_level()) -- when max_level is implemneted properly
	vim.b.ufo_foldlevel = math.max(0, num)
	require('ufo').closeFoldsWith(vim.b.ufo_foldlevel)
end

---Shift the current fold level by the provided amount
---@param dir number positive or negative number to add to the current fold level to shift it
local shift_fold = function(dir) set_fold((vim.b.ufo_foldlevel or max_level()) + dir) end

local hydra_ok, Hydra = pcall(require, 'hydra')
if not hydra_ok then
	print 'could not load hydra'
	return
end

local fold_hyrda = Hydra {
	name = 'Tab Jumper',
	mode = 'n',
	hint = [[
 Params:
 move: ← _h_ → _l_
 exit: _j_
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
		{ 'h', function() shift_fold(-(vim.v.count == 0 and 1 or vim.v.count)) end },
		{ 'l', function() shift_fold(vim.v.count == 0 and 1 or vim.v.count) end },
	},
}

-- TODO: some more cool plugins:
-- https://github.com/mfussenegger/nvim-treehopper

return {
	fold_hyrda = fold_hyrda,
}
