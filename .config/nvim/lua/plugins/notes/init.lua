local constants = require 'plugins.notes.constants'

return {
	{
		'mzlogin/vim-markdown-toc',
		init = function() vim.cmd [[let g:vmt_auto_update_on_save = 0]] end,
	},

	{
		'iamcco/markdown-preview.nvim',
		cmd = { 'MarkdownPreviewToggle', 'MarkdownPreview', 'MarkdownPreviewStop' },
		build = 'sh -c "cd app yarn install"',
		init = function()
			vim.g.mkdp_browser = 'firefox'
			vim.g.mkdp_filetypes = { 'markdown' }
			vim.g.mkdp_refresh_slow = 1 -- too much jumping around, refresh on save or insert leave
		end,
		ft = { 'markdown' },
	},

	{
		'epwalsh/obsidian.nvim',
		version = '*', -- recommended, use latest release instead of latest commit
		lazy = true,
		cond = constants.has_notes,
		event = {
			'BufReadPre ' .. constants.pattern,
			'BufNewFile ' .. constants.pattern,
		},
		dependencies = { 'nvim-lua/plenary.nvim' },
		cmd = { 'ObsidianWorkspace', 'ObsidianToday', 'ObsidianQuickSwitch' },
		init = function()
			-- set.conceallevel = 0 -- So that I can see `` in markdown files
			vim.opt.conceallevel = 2 -- while trying obsidian
			-- stylua: ignore
			U.keys('markdown', {
				{ 'j', function() require('plugins.notes.fns').table_swap_down() end, 'Table ⬇', },
				{ 'k', function() require('plugins.notes.fns').table_swap_up() end, 'Table ⬆' },
				{ 'h', function() require('plugins.notes.fns').table_swap_left() end, 'Table ⇽' },
				{ 'l', function() require('plugins.notes.fns').table_swap_right() end, 'Table ⇾', },
			})
		end,
		opts = {
			workspaces = {
				U.machine {
					work = {
						name = 'work',
						path = constants.cwd,
					},
					home = {
						name = 'personal',
						path = constants.cwd,
					},
				},
			},

			notes_subdir = 'inbox',

			daily_notes = {
				folder = 'journal',
				template = 'daily.md',
			},

			new_notes_location = 'notes_subdir',
			-- new_notes_location = 'current_dir', --inline with PARA

			completion = {
				min_chars = 1,
			},

			-- open notes split if there isn't already a split
			open_notes_in = 'vsplit',

			follow_url_func = function(url) vim.fn.jobstart { 'open', url } end,

			templates = {
				subdir = 'templates',
				date_format = '%Y-%m-%d',
				time_format = '%H:%M',
				substitutions = {
					-- TODO ObsidianTomorrow will have wrong date, should use a hacky autcmd to just replace everything
					friendly_date = function() return os.date '%B %-d, %Y' end,
				},
			},

			-- wiki_link_func = function(opts)
			-- 	return require('plugins.notes.fns').wiki_link_func(opts)
			-- end,

			-- this prevents new files having silly names
			note_id_func = function(title) return title end,

			disable_frontmatter = true,

			callbacks = {
				post_setup = function() require('plugins.notes.backup').init() end,

				-- Runs right before writing the buffer for a note.
				---@param _ obsidian.Client
				---@param note obsidian.Note
				pre_write_note = function(_, note)
					local fname = note:fname()
					require('plugins.notes.fns').check_name_clash { fname }
				end,
			},

			picker = {
				mappings = { new = '<C-n>' },
			},
			ui = {
				hl_groups = {
					-- The options are passed directly to `vim.api.nvim_set_hl()`. See `:help nvim_set_hl`.
					ObsidianTodo = { bold = true, fg = '#f78c6c' },
					ObsidianDone = { bold = true, fg = '#89ddff' },
					ObsidianRightArrow = { bold = true, fg = '#f78c6c' },
					ObsidianTilde = { bold = true, fg = '#ff5370' },
					ObsidianBullet = { bold = true, fg = '#89ddff' },
					ObsidianRefText = { underline = true, fg = '#c792ea' },
					ObsidianExtLinkIcon = { fg = '#c792ea' },
					ObsidianTag = { italic = true, fg = '#89ddff' },
					ObsidianBlockID = { italic = true, fg = '#89ddff' },
					-- ObsidianHighlightText = { bg = "#75662e" },

					-- NOTE: shouldn't need all the others, but they don't get merged
					ObsidianHighlightText = {
						bg = require('rose-pine.palette').rose,
						fg = require('rose-pine.palette').base,
					},
				},
			},
		},
	},
	{
		'lukas-reineke/headlines.nvim',
		ft = 'markdown',
		dependencies = {
			'nvim-treesitter/nvim-treesitter',
			U.highlights {
				Headline = { bg = 'surface' },
				CodeBlock = { bg = 'surface' },
			},
		},
		config = true, -- or `opts = {}`
	},
}
