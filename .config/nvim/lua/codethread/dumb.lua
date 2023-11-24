local M = {}

function M.get_word()
	local word = vim.fn.expand '<cword>'
	local v = vim.fn.jobstart({ 'rg', word }, {
		stdout_buffered = true,
		stderr_buffered = true,
		on_stderr = function(_, code)
			vim.notify('fail ' .. code)
		end,
		on_stdout = function(a, b, c, d)
			vim.notify 'hi'
			vim.print {
				a = a,
				b = b,
				c = c,
				d = d,
			}
		end,
	})
	vim.notify('hey ' .. v)
end

U.keymap('n', 'gd', M.get_word, 'go to def')
