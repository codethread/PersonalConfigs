local M = {}

local function set_signs()
	local signs = {
		{ name = "DiagnosticSignError", text = "" },
		{ name = "DiagnosticSignWarn", text = "" },
		{ name = "DiagnosticSignHint", text = "" },
		{ name = "DiagnosticSignInfo", text = "" },
	}

	for _, sign in ipairs(signs) do
		vim.fn.sign_define(sign.name, { texthl = sign.name, text = sign.text, numhl = "" })
	end

	return signs
end

M.setup = function()
	vim.diagnostic.config({

		virtual_text = true,
		underline = false,

		signs = {
			active = set_signs(),
		},
		update_in_insert = false, -- update on InsertLeave
		severity_sort = true,
		float = {
			focusable = false,
			style = "minimal",
			border = "solid", -- see :h nvim_open_win()
			source = "always",
			header = "",
			prefix = "",
		},
	})
end

return M
