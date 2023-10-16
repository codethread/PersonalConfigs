-- attempted to get conjure to work, but repl doesn't like not being in a TTY
-- probalby need to spawn in :terminal and communicate that way
local a = require 'conjure.aniseed.core'
local config = require 'conjure.config'
local client = require 'conjure.client'
local stdio = require 'conjure.remote.stdio'
local log = require 'conjure.log'

config.merge {
	client = {
		nu = {
			stdio = {
				mapping = { start = 'cs', stop = 'cS', interrupt = 'ei' },
				command = 'nu',
				prompt_pattern = '>> ',
			},
		},
	},
}

local comment_prefix = '# '
local cfg = config['get-in-fn'] { 'client', 'nu', 'stdio' }
local state = client['new-state'](function(opts)
	vim.print(opts)
	return { repl = nil }
end)

local function with_repl_or_warn(fn, opts)
	local repl = state 'repl'
	if repl then
		fn(repl)
	else
		return log.append {
			(comment_prefix .. 'No REPL running'),
			(
				comment_prefix
				.. 'Start REPL with '
				.. config['get-in'] { 'mapping', 'prefix' }
				.. cfg { 'mapping', 'start' }
			),
		}
	end
end

local function start()
	if state 'repl' then
		log.append({
			comment_prefix .. "Can't start, REPL is already running.",
			comment_prefix .. 'stop the REPL with... ',
		}, {
			['break'] = true,
		})
	else
		return a.assoc(
			state(),
			'repl',
			stdio.start {
				['prompt-pattern'] = cfg { 'prompt_pattern' },
				cmd = 'nu -l',
				['on-success'] = function()
					vim.print 'success'
					with_repl_or_warn(function(repl)
						repl.send('help:\n', function(msgs)
							vim.print(msgs)
							return msgs
						end)
					end, opts)
				end,
				['on-exit'] = function(code, signal)
					vim.print('exit ' .. code .. ' ' .. (signal or ''))
				end,
				['on-error'] = function(code, signal)
					vim.print('error ' .. code .. ' ' .. (signal or ''))
				end,
				['on-stray-output'] = function(msg) vim.print('stray' .. msg) end,
			}
		)
	end
end

local api = {
	['buf-suffix'] = '.nu',
	['comment-prefix'] = comment_prefix,
	['context-pattern'] = nil, -- ??
	['def-str'] = nil, -- probably just want dumb-jump
	completions = nil, -- later
	['eval-str'] = function(opts)
		vim.print 'eval str'
		vim.print(opts)
	end,
	['doc-str'] = function(opts)
		vim.print 'doc'
		vim.print(opts)
	end,
	['eval-file'] = function(opts)
		vim.print 'file'
		vim.print(opts)
	end,
	['connect'] = function() vim.print 'heyyeyeyye' end,
	['on-filetype'] = function()
		-- TODO mappings and stuff
	end,
	['on-load'] = function()
		vim.print 'START'
		start()
	end,
	['on-exit'] = function() vim.print 'EXIT' end,
}

return api
