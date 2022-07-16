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
