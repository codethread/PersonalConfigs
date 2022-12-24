local qmk_status_ok, qmk = pcall(require, 'qmk')
if not qmk_status_ok then
	print 'could not load qmk'
	return
end

qmk.setup {
	columns = 12,
	rows = 5,
}
