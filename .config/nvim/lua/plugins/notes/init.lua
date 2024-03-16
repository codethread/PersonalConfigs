return {
	{ 'mzlogin/vim-markdown-toc' },
	{
		'epwalsh/obsidian.nvim',
		version = '*', -- recommended, use latest release instead of latest commit
		lazy = true,
		ft = 'markdown',
		-- Replace the above line with this if you only want to load obsidian.nvim for markdown files in your vault:
		-- event = {
		--   -- If you want to use the home shortcut '~' here you need to call 'vim.fn.expand'.
		--   -- E.g. "BufReadPre " .. vim.fn.expand "~" .. "/my-vault/**.md"
		--   "BufReadPre path/to/my-vault/**.md",
		--   "BufNewFile path/to/my-vault/**.md",
		-- },
		dependencies = { 'nvim-lua/plenary.nvim' },
		cmd = { 'ObsidianWorkspace', 'ObsidianToday', 'ObsidianQuickSwitch' },
		init = function()
			-- set.conceallevel = 0 -- So that I can see `` in markdown files
			vim.opt.conceallevel = 2 -- while trying obsidian
		end,
		opts = {
			workspaces = {
				U.machine {
					work = {
						name = 'work',
						path = '~/gdrive/perks',
					},
					home = {
						name = 'personal',
						path = '~/Library/Mobile Documents/iCloud~md~obsidian/Documents/Notes',
					},
				},
			},

			notes_subdir = 'notes',

			daily_notes = {
				folder = 'journal',
				template = 'daily.md',
			},

			new_notes_location = 'notes_subdir',

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

			-- Optional, customize how wiki links are formatted.
			---@param opts {path: string, label: string, id: string|?}
			---@return string
			wiki_link_func = function(opts) return string.format('[[%s]]', opts.label) end,

			-- not into zettlcrapsten so a title is fine. Maybe should remove bad chars though or panic
			note_id_func = function(title) return title end,

			-- Optional, alternatively you can customize the frontmatter data.
			note_frontmatter_func = function(note)
				-- TODO seems to include the title in the alias
				-- docs to obsidian.Note
				-- ~/.local/share/nvim/lazy/obsidian.nvim/doc/obsidian_api.txt:349
				-- This is equivalent to the default frontmatter function.
				local out = { id = note.id, aliases = note.aliases, tags = note.tags }
				-- `note.metadata` contains any manually added fields in the frontmatter.
				-- So here we just make sure those fields are kept in the frontmatter.
				if note.metadata ~= nil and not vim.tbl_isempty(note.metadata) then
					for k, v in pairs(note.metadata) do
						out[k] = v
					end
				end
				return out
			end,
			callbacks = {
				post_setup = function() require('plugins.notes.backup').init() end,
			},
		},
	},
}
