local status_ok, hop = pcall(require, "hop")
if not status_ok then
	print("could not load hop")
	return
end

-- you can configure Hop the way you like here; see :h hop-config
hop.setup({ keys = "fjdksla;rucnei" })

-- place this in one of your configuration file(s)
vim.api.nvim_set_keymap(
	"",
	"f",
	"<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.AFTER_CURSOR, current_line_only = true })<cr>",
	{}
)
vim.api.nvim_set_keymap(
	"",
	"F",
	"<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.BEFORE_CURSOR, current_line_only = true })<cr>",
	{}
)
vim.api.nvim_set_keymap(
	"",
	"t",
	"<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.AFTER_CURSOR, current_line_only = true, hint_offset = -1 })<cr>",
	{}
)
vim.api.nvim_set_keymap(
	"",
	"T",
	"<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.BEFORE_CURSOR, current_line_only = true, hint_offset = 1 })<cr>",
	{}
)

vim.api.nvim_set_keymap("n", "s", "<cmd>lua require'hop'.hint_words({ jump_on_sole_occurrence = true })<cr>", {})
vim.api.nvim_set_keymap(
	"n",
	"S",
	"<cmd>lua require'hop'.hint_words({ jump_on_sole_occurrence = true, multi_windows = true })<cr>",
	{}
)

local hydra_ok, Hydra = pcall(require, "hydra")
if not hydra_ok then
	print("could not load hydra")
	return
end

local function cmd(command)
	return table.concat({ "<Cmd>", command, "<CR>" })
end

local mover_hint = [[
 Params:
 move: ← _h_ → _l_
 goto: ↓ _j_ ↑ _k_
]]

local mover_hydra = Hydra({
	name = "Param Mover",
	mode = "n",
	hint = mover_hint,
	config = {
		invoke_on_body = true,
	},
	heads = {
		{ "j", cmd("TSTextobjectGotoNextStart @parameter.inner") },
		{ "k", cmd("TSTextobjectGotoPreviousStart @parameter.inner") },
		{ "h", cmd("TSTextobjectSwapPrevious @parameter.inner") },
		{ "l", cmd("TSTextobjectSwapNext @parameter.inner") },
	},
})

-- TODO: some more cool plugins:
-- https://github.com/mfussenegger/nvim-treehopper

return {
	mover_hydra = mover_hydra,
}
