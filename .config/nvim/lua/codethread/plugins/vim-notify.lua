local safe_load = require('codethread.utils').safe_load

safe_load('notify', function(notify)
	notify.setup {
		background_colour = '#000000',
		max_width = 100,
		stages = 'slide',
		timeout = 500,
	}

	--[[
  silence annoying errors from lsp's that have no hover information
  source https://github.com/neovim/nvim-lspconfig/issues/1931#issuecomment-1297599534
  --]]
	local banned_messages = {
		'No information available',
		'warning: multiple different client offset_encodings detected for buffer, this is not supported yet',
	}
	vim.notify = function(msg, ...)
		for _, banned in ipairs(banned_messages) do
			if msg == banned then return end
		end

		notify(msg, ...)
	end
end)
