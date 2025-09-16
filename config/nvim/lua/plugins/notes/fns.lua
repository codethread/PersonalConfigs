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
		fd --extension=md --type=f -E 'assets' -E excalidraw -E templates
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

-- yank from the current table (between pipes)
-- relies on using registers for now, could always be more fancy
-- with lua and treesitter another day
function M.table_swap_down() vim.cmd [[noau norm "tyi|j"yyi|ci|tkci|yjF|wlh]] end
function M.table_swap_up() vim.cmd [[noau norm "tyi|k"yyi|ci|tjci|ykF|wlh]] end
function M.table_swap_left() vim.cmd [[noau norm "tyi|2T|"yyi|ci|t2t|ci|y2T|wlh]] end
function M.table_swap_right() vim.cmd [[noau norm "tyi|2t|"yyi|ci|t2T|ci|yf|wlh]] end

--- Lifted from the original but tweaks the anchor to be compatible with the app
---
--- NOTE: this has the drawback of not creating links as id|alias which breaks obsidian
--- origianlly set up so i could write a lowercase name for an uppercase file, but I should
--- probably fix this
---
---@param opts { path: string, label: string, id: string|integer|?, anchor: obsidian.note.HeaderAnchor|?, block: obsidian.note.Block|? }
---@return string
function M.wiki_link_func(opts)
	local header = ''
	local formatted_header = ''
	-- local name = opts.id or opts.label
	local name = opts.label

	if opts.anchor then
		header = string.format('#%s', opts.anchor.header)
		formatted_header = string.format(' ❯ %s', opts.anchor.header)
	elseif opts.block then
		header = '#' .. opts.block.id
		formatted_header = '#' .. opts.block.id
	end

	if header == '' then
		return string.format('[[%s]]', name)
	else
		return string.format('[[%s%s|%s%s]]', name, header, name, formatted_header)
	end
end

return M
