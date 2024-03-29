local constants = require 'plugins.notes.constants'
local M = {}

---Open the given note file
function M.notes_path_to(file)
	vim.cmd.e(require('plugins.notes')[2].opts.workspaces[1].path .. '/notes/' .. file)
end

---Creates a table of contents for obsidian files
function M.create_markdown_toc()
	-- set a mark
	-- move to the top of the file and search for the end of the frontmatter
	vim.cmd [[silent execute "normal mzgg/---/e+1\<CR>n"]]
	local needs_creating = vim.fn.search('vim-markdown-toc', 'wn') == 0
	if needs_creating then
		vim.cmd [[silent GenTocGFM]]
	else
		vim.cmd [[silent UpdateToc]]
	end
	-- go back to the mark and center the buffer
	vim.cmd [[silent execute "norm 'zzz"]]
end

function M.rename()
	vim.ui.input({ prompt = 'New file name: ' }, function(choice)
		if not choice or choice == '' then return end
		vim.notify(choice)
		-- r = { Cmd 'ObsidianRename'}
	end)
end

---Check async that no duplicate file names have been created
---@param files string[]
function M.check_name_clash(files)
	U.nush(
		[[
	fd --extension=md --type=f -E 'assets' -E excalidraw
	| lines
	| path parse
	| uniq-by --repeated stem | get stem
	| if (($in | length) > 0) {
	  error make --unspanned { msg: $"The following files are duplicated\n($in | to text)" }
	}
]],
		{ cwd = constants.cwd },
		function(out)
			if out.code ~= 0 then
				vim.notify(out.stderr, vim.log.levels.ERROR, {
					title = 'ct.Obsidian',
				})
			end
		end
	)
end

return M
