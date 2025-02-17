local function set_signs()
	local signs = {
		{ name = 'DiagnosticSignError', text = '' },
		{ name = 'DiagnosticSignWarn', text = '' },
		{ name = 'DiagnosticSignHint', text = '' },
		{ name = 'DiagnosticSignInfo', text = '' },
	}

	for _, sign in ipairs(signs) do
		vim.fn.sign_define(sign.name, { texthl = sign.name, text = sign.text, numhl = '' })
	end

	return signs
end

local function format(diagnostic)
	if diagnostic.source == 'eslint_d' then
		return string.format(
			'%s [%s]',
			diagnostic.message,
			-- shows the name of the rule
			diagnostic.code
		)
	end
	return string.format('%s [%s]', diagnostic.message, diagnostic.source)
end

vim.diagnostic.config {
	severity_sort = true,
	signs = { active = set_signs() },
	underline = true,
	update_in_insert = false,
	virtual_text = { spacing = 4, source = 'if_many', prefix = '●' },
	float = {
		focusable = false,
		style = 'minimal',
		border = 'solid', -- see :h nvim_open_win()
		source = true,
		header = '',
		prefix = '',
		format = format,
	},
}

local M = {}
function M.next_diagnostic()
	local ft = vim.bo.filetype
	vim.diagnostic.jump {
		severity = ft ~= 'lua' and vim.diagnostic.severity.ERROR or vim.diagnostic.severity.WARN,
		count = 1,
		float = true,
	}
end

function M.previous_diagnostic()
	local ft = vim.bo.filetype
	vim.diagnostic.jump {
		severity = ft ~= 'lua' and vim.diagnostic.severity.ERROR or vim.diagnostic.severity.WARN,
		count = -1,
		float = true,
	}
end
return M
