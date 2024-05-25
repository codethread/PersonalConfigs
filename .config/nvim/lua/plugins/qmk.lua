return {
	{
		'codethread/qmk.nvim',
		lazy = true,
		init = function()
			vim.filetype.add {
				extension = {
					keymap = 'dts',
				},
			}

			local group = vim.api.nvim_create_augroup('MyQMK', {})

			vim.api.nvim_create_autocmd('BufEnter', {
				desc = 'Format qmk keymap',
				group = group,
				pattern = '*/keymap.c',
				callback = function()
					---@type qmk.UserConfig
					local conf = {
						name = 'LAYOUT_preonic_grid',
						layout = {
							'_ x x x x x x _ x x x x x x',
							'_ x x x x x x _ x x x x x x',
							'_ x x x x x x _ x x x x x x',
							'_ x x x x x x _ x x x x x x',
							'_ x x x x x x _ x x x x x x',
						},
						comment_preview = {
							keymap_overrides = {
								KC_LSFT = '󰘶',
								KC_LALT = '⎇',
								KC_LCTL = '^',
								KC_LGUI = '󰘳',
								LSFT_T = '󰘶',
								LALT_T = '⎇',
								LCTL_T = '^',
								LGUI_T = '󰘳',
							},
						},
					}
					require('qmk').setup(conf)
				end,
			})

			vim.api.nvim_create_autocmd('BufEnter', {
				desc = 'Format zmk keymap',
				group = group,
				pattern = '*/dao.keymap',
				callback = function()
					---@type qmk.UserConfig
					local zmk_conf = {
						name = 'hi',
						variant = 'zmk',
						auto_format_pattern = '*/dao.keymap',
						comment_preview = { position = 'none' },
						layout = {
							'_ x x x x x x _ x x x x x x',
							'_ x x x x x x _ x x x x x x',
							'_ x x x x x x _ x x x x x x',
							'_ _ _ _ x x x _ x x x _ _ _',
						},
					}
					require('qmk').setup(zmk_conf)
				end,
			})
		end,
	},
}
