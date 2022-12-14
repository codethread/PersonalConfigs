vim.o.foldlevel = 99 -- Using ufo provider need a large value, feel free to decrease the value
vim.o.foldlevelstart = 99
vim.o.foldenable = true

-- hide foldcolumn
vim.o.foldcolumn = '0'
-- or show with
-- vim.o.foldcolumn = '1'
-- vim.o.fillchars = [[eob: ,fold: ,foldopen:,foldsep: ,foldclose:]] -- add nice fold icons

local utils = require 'codethread.utils'
local nmap = utils.nmap

-- Using ufo provider need remap `zR` and `zM`. If Neovim is 0.6.1, remap yourself
nmap('zR', require('ufo').openAllFolds)
nmap('zM', require('ufo').closeAllFolds)
nmap('zp', require('ufo').peekFoldedLinesUnderCursor)

nmap('-', 'zc', { desc = 'open fold under cursor' })
nmap('=', 'zo', { desc = 'close fold under cursor' })
nmap('_', 'zC', { desc = 'close all folds under cursor' })
nmap('+', 'zO', { desc = 'open all folds under cursor' })

local handler = function(virtText, lnum, endLnum, width, truncate)
	local newVirtText = {}
	local suffix = ('  %d '):format(endLnum - lnum)
	local sufWidth = vim.fn.strdisplaywidth(suffix)
	local targetWidth = width - sufWidth
	local curWidth = 0
	for _, chunk in ipairs(virtText) do
		local chunkText = chunk[1]
		local chunkWidth = vim.fn.strdisplaywidth(chunkText)
		if targetWidth > curWidth + chunkWidth then
			table.insert(newVirtText, chunk)
		else
			chunkText = truncate(chunkText, targetWidth - curWidth)
			local hlGroup = chunk[2]
			table.insert(newVirtText, { chunkText, hlGroup })
			chunkWidth = vim.fn.strdisplaywidth(chunkText)
			-- str width returned from truncate() may less than 2nd argument, need padding
			if curWidth + chunkWidth < targetWidth then
				suffix = suffix .. (' '):rep(targetWidth - curWidth - chunkWidth)
			end
			break
		end
		curWidth = curWidth + chunkWidth
	end
	table.insert(newVirtText, { suffix, 'MoreMsg' })
	return newVirtText
end

require('ufo').setup {
	provider_selector = function() return { 'treesitter', 'indent' } end,
	fold_virt_text_handler = handler,
}
