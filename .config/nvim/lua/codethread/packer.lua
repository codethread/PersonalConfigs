local fn = vim.fn
local install_path = fn.stdpath 'data' .. '/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
	PACKER_BOOTSTRAP = fn.system {
		'git',
		'clone',
		'--depth',
		'1',
		'https://github.com/wbthomason/packer.nvim',
		install_path,
	}
	vim.cmd 'packadd packer.nvim'
end

if not vim.fn.executable 'python' then
	vim.notify "no python, make sure to link it, e.g 'ln -s /usr/local/bin/python3 /usr/local/bin/python'"
end
