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

return M
