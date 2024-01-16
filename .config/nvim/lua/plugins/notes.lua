return {
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
	dependencies = {
		-- Required.
		'nvim-lua/plenary.nvim',

		-- see below for full list of optional dependencies 👇
	},
	init = function()
		-- set.conceallevel = 0 -- So that I can see `` in markdown files
		vim.opt.conceallevel = 2 -- while trying obsidian
	end,
	cmd = { 'ObsidianWorkspace', 'ObsidianToday' },
	opts = {
		workspaces = {
			{
				name = 'personal',
				path = '~/Library/Mobile Documents/iCloud~md~obsidian/Documents/Test',
			},
			{
				name = 'work',
				path = '~/gdrive/perks',
			},
		},

		notes_subdir = 'notes',
		daily_notes = {
			-- Optional, if you keep daily notes in a separate directory.
			folder = 'journal',
		},

		-- Optional, customize how names/IDs for new notes are created.
		-- TODO figure out if an id is needed
		note_id_func = function(title)
			-- Create note IDs in a Zettelkasten format with a timestamp and a suffix.
			-- In this case a note with the title 'My new note' will be given an ID that looks
			-- like '1657296016-my-new-note', and therefore the file name '1657296016-my-new-note.md'
			local suffix = ''
			if title ~= nil then
				-- If title is given, transform it into valid file name.
				suffix = title:gsub(' ', '-'):gsub('[^A-Za-z0-9-]', ''):lower()
			else
				-- If title is nil, just add 4 random uppercase letters to the suffix.
				for _ = 1, 4 do
					suffix = suffix .. string.char(math.random(65, 90))
				end
			end
			return tostring(os.time()) .. '-' .. suffix
		end,

		-- see below for full list of options 👇
	},
}
