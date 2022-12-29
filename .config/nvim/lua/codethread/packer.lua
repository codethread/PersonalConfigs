local M = {}

if not vim.fn.executable 'python' then
	vim.notify "no python, make sure to link it, e.g 'ln -s /usr/local/bin/python3 /usr/local/bin/python'"
end

function M.ensure_packer()
	local fn = vim.fn
	local install_path = fn.stdpath 'data' .. '/site/pack/packer/start/packer.nvim'
	if fn.empty(fn.glob(install_path)) > 0 then
		fn.system {
			'git',
			'clone',
			'--depth',
			'1',
			'https://github.com/wbthomason/packer.nvim',
			install_path,
		}
		vim.cmd [[packadd packer.nvim]]
		return true
	end
	return false
end

function M.use_local_partial(use)
	local function use_local(path)
		if require('codethread.utils').file_exits(path) then
			use(path)
		else
			local segs = vim.split(path, '/')
			use('codethread/' .. segs[#segs])
		end
	end

	return use_local
end

return M
