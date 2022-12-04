local status_ok, glow = pcall(require, 'glow')
if not status_ok then
	print 'could not load glow'
	return
end

glow.setup {
	border = 'single',
	-- your override config
}
