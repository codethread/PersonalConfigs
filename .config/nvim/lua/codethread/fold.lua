local M = {}

-- Ensure our ufo foldlevel is set for the buffer
vim.api.nvim_create_autocmd('BufReadPre', {
	callback = function() vim.b.ufo_foldlevel = 0 end,
})

---@param num integer The amount to change the UFO fold level by
local change_buf_foldlevel_by = function(num)
	local foldlevel = vim.b.ufo_foldlevel or 0
	-- Ensure the foldlevel can't be set negatively
	if foldlevel + num >= 0 then
		foldlevel = foldlevel + num
	else
		foldlevel = 0
	end
	M.fold_setlevel(foldlevel)
end

---@param num integer Set the fold level to this number
function M.fold_setlevel(num)
	vim.b.ufo_foldlevel = num
	require('ufo').closeFoldsWith(num)
end

function M.fold_increase()
	local count = vim.v.count
	if count == 0 then count = 1 end
	change_buf_foldlevel_by(-count)
end

function M.fold_decrease()
	local l = vim.fn.winsaveview()
	local count = vim.v.count
	if count == 0 then count = 1 end
	change_buf_foldlevel_by(count)
	-- vim.cmd 'norm zz'
	vim.fn.winrestview(l)
end

local hydra_ok, Hydra = pcall(require, 'hydra')
if not hydra_ok then
	print 'could not load hydra'
	return
end

M.fold_hyrda = Hydra {
	name = 'Tab Jumper',
	mode = 'n',
	hint = [[
 Params:
 move: ← _h_ _l_ → 
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
		{ 'h', function() M.fold_increase() end },
		{ 'l', function() M.fold_decrease() end },
	},
}

-- TODO: some more cool plugins:
-- https://github.com/mfussenegger/nvim-treehopper

return M
