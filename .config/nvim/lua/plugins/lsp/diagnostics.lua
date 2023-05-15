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

local diagnostics_active = true
local function toggle_diagnostic()
	diagnostics_active = not diagnostics_active
	if diagnostics_active then
		vim.diagnostic.show()
	else
		vim.diagnostic.hide()
	end
end

return {
	{
		'neovim/nvim-lspconfig',
		keys = {
			{ '<leader>eh', toggle_diagnostic, desc = 'Toggle Diagnostics' },
			{ '<leader>el', Cmd 'Telescope diagnostics theme=ivy bufnr=0', desc = 'Document Diagnostics' },
			{ '<leader>eL', Cmd 'Telescope diagnostics', desc = 'Workspace Diagnostics' },
			{ '<leader>en', Cmd 'lua vim.diagnostic.goto_next()', desc = 'Next Diagnostic' },
			{ '<leader>ep', Cmd 'lua vim.diagnostic.goto_prev()', desc = 'Prev Diagnostic' },
			{ '<leader>eq', Cmd 'lua vim.diagnostic.setloclist()', desc = 'Quickfix' },
		},
		opts = function(_, opts)
			return vim.tbl_deep_extend('force', opts, {
				diagnostics = {
					severity_sort = true,
					signs = { active = set_signs() },
					underline = true,
					update_in_insert = false,
					virtual_text = { spacing = 4, source = 'if_many', prefix = '●' },
					float = {
						focusable = false,
						style = 'minimal',
						border = 'solid', -- see :h nvim_open_win()
						source = 'always',
						header = '',
						prefix = '',
						format = format,
					},
				},
			})
		end,
	},
}
