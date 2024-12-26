return {
	{
		'folke/tokyonight.nvim',
		enabled = false,
		opts = {
			style = 'storm',
			light_style = 'day',
			day_brightness = 0.25,

			-- transparent = true, -- if i want a nice terminal background
			transparent = false, -- needed for better background control
			-- dim_inactive = true, -- cool, but if using other dim windows, this gets annoying

			hide_inactive_statusline = true,

			sidebars = { 'qf', 'help', 'packer' }, -- Set a darker background on sidebar-like windows. For example: `["qf", "vista_kind", "terminal", "packer"]`

			styles = {
				-- Style to be applied to different syntax groups
				-- Value is any valid attr-list value for `:help nvim_set_hl`
				comments = { italic = true },
				-- Background styles. Can be "dark", "transparent" or "normal"
				sidebars = 'dark', -- style for sidebars, see below
				floats = 'dark', -- style for floating windows
			},

			on_highlights = function(hl, c)
				hl.LineNr = { fg = c.green2 }
				-- fix highlights in embeddedLanguages, i think by default this falls through to lower specificty selectors, and because these are embedded, that falls to 'string', so everything goes green
				hl['@variable'] = { fg = c.fg }

				-- rust/cargo
				hl['@table.package'] = { fg = c.red }

				hl['@variable.builtin'] = { fg = c.teal, bold = true }
				hl['@keyword.return'] = { fg = c.red }
				hl['@keyword.bang'] = { fg = c.red, bold = true, underline = true }

				-- js stuff
				hl['@arrow_function.const'] = { underline = true }
				hl['@keyword.export'] = { fg = c.red }
				hl['@keyword.default'] = { fg = c.red, bold = true }

				hl.CursorLineNr = { fg = c.green1 }

				-- borderless telescope
				local prompt = '#2d3149'
				hl.TelescopeNormal = {
					bg = c.bg_dark,
					fg = c.fg_dark,
				}
				hl.TelescopeBorder = {
					bg = c.bg_dark,
					fg = c.bg_dark,
				}
				hl.TelescopePromptNormal = {
					bg = prompt,
				}
				hl.TelescopePromptBorder = {
					bg = prompt,
					fg = prompt,
				}
				hl.TelescopePromptTitle = {
					bg = prompt,
					fg = prompt,
				}
				hl.TelescopePreviewTitle = {
					bg = c.bg_dark,
					fg = c.bg_dark,
				}
				hl.TelescopeResultsTitle = {
					bg = c.bg_dark,
					fg = c.bg_dark,
				}

				-- hydra
				hl.HydraRed = { fg = '#FF5733' }
				hl.HydraBlue = { fg = '#5EBCF6' }
				hl.HydraAmaranth = { fg = '#ff1757' }
				hl.HydraTeal = { fg = '#00a1a1' }
				hl.HydraPink = { fg = '#ff55de' }

				-- autocomplete
				-- TODO: get these from theme
				hl.PmenuSel = { bg = '#282C34', fg = 'NONE' }
				hl.Pmenu = { fg = '#C5CDD9', bg = '#22252A' }

				hl.CmpItemAbbrDeprecated = { fg = '#7E8294', bg = 'NONE', strikethrough = true }
				hl.CmpItemAbbrMatch = { fg = '#82AAFF', bg = 'NONE', bold = true }
				hl.CmpItemAbbrMatchFuzzy = { fg = '#82AAFF', bg = 'NONE', bold = true }
				hl.CmpItemMenu = { fg = '#C792EA', bg = 'NONE', italic = true }

				hl.CmpItemKindField = { fg = '#EED8DA', bg = '#B5585F' }
				hl.CmpItemKindProperty = { fg = '#EED8DA', bg = '#B5585F' }
				hl.CmpItemKindEvent = { fg = '#EED8DA', bg = '#B5585F' }

				hl.CmpItemKindText = { fg = '#C3E88D', bg = '#9FBD73' }
				hl.CmpItemKindEnum = { fg = '#C3E88D', bg = '#9FBD73' }
				hl.CmpItemKindKeyword = { fg = '#C3E88D', bg = '#9FBD73' }

				hl.CmpItemKindConstant = { fg = '#FFE082', bg = '#D4BB6C' }
				hl.CmpItemKindConstructor = { fg = '#FFE082', bg = '#D4BB6C' }
				hl.CmpItemKindReference = { fg = '#FFE082', bg = '#D4BB6C' }

				hl.CmpItemKindFunction = { fg = '#EADFF0', bg = '#A377BF' }
				hl.CmpItemKindStruct = { fg = '#EADFF0', bg = '#A377BF' }
				hl.CmpItemKindClass = { fg = '#EADFF0', bg = '#A377BF' }
				hl.CmpItemKindModule = { fg = '#EADFF0', bg = '#A377BF' }
				hl.CmpItemKindOperator = { fg = '#EADFF0', bg = '#A377BF' }

				hl.CmpItemKindVariable = { fg = '#C5CDD9', bg = '#7E8294' }
				hl.CmpItemKindFile = { fg = '#C5CDD9', bg = '#7E8294' }

				hl.CmpItemKindUnit = { fg = '#F5EBD9', bg = '#D4A959' }
				hl.CmpItemKindSnippet = { fg = '#F5EBD9', bg = '#D4A959' }
				hl.CmpItemKindFolder = { fg = '#F5EBD9', bg = '#D4A959' }

				hl.CmpItemKindMethod = { fg = '#DDE5F5', bg = '#6C8ED4' }
				hl.CmpItemKindValue = { fg = '#DDE5F5', bg = '#6C8ED4' }
				hl.CmpItemKindEnumMember = { fg = '#DDE5F5', bg = '#6C8ED4' }

				hl.CmpItemKindInterface = { fg = '#D8EEEB', bg = '#58B5A8' }
				hl.CmpItemKindColor = { fg = '#D8EEEB', bg = '#58B5A8' }
				hl.CmpItemKindTypeParameter = { fg = '#D8EEEB', bg = '#58B5A8' }

				hl.CmpItemKindCopilot = { fg = '#D8EEEB', bg = '#58B5A8' }

				hl.IndentBlanklineChar = { fg = c.fg_gutter }
				hl.IndentBlanklineContextChar = { fg = c.magenta }
			end,
		},
	},
	{
		'shaunsingh/nord.nvim',
		enabled = false,
		config = function()
			local function colors()
				-- TODO make this easier to consume with some types perhaps??
				return function()
					local t = require 'nord.colors'
					local c = require 'nord.named_colors'
					local dark = '#242933'

					return {
						t = t,
						c = c,
						dark = dark,
					}
				end
			end

			vim.g.nord_contrast = true
			vim.g.nord_borders = true
			vim.g.nord_disable_background = false
			vim.g.nord_italic = true
			vim.g.nord_uniform_diff_background = true
			vim.g.nord_bold = false

			local theme = colors() or {}
			local t = theme.t
			local c = theme.c
			local dark = theme.dark
			local utils = require 'codethread.utils'
			local hl = utils.hl

			t.float = theme.dark
			nord.set()

			hl {
				LineNr = { fg = c.teal },
				-- FoldColumn = { fg = c.dark_gray },
				NormalDark = { bg = dark },
				NvimTreeNormal = { bg = dark },
				TelescopeSelection = { bg = t.nord1_gui },
				['@keyword.return'] = { fg = c.orange },
			}

			vim.cmd [[
			augroup NordHighlights
			au!
			au FileType help setlocal winhighlight=Normal:NormalDark,SignColumn:NormalDark
			augroup end
			]]
		end,
	},
}
