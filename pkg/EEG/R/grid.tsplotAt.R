grid.tsplotAt <-
function(ts, x, y, width, height, just="centre", name, xlim=NA, ylim=NA,
	angle = 0, add=FALSE, ..., box=FALSE, axes=FALSE, ylab = "", rot=90, fontsize=7)
{
	t = time(ts)
	if(is.na(xlim))
		xlim = range(t)
	if(is.na(ylim))
		ylim = range(ts)
	if(missing(name))
		name = "vp1"
	if(!add){
		vp0 = viewport(x,y,unit(width, "npc"),unit(height,"npc"), default.units="native",just=just)
		vp1 = dataViewport(xlim,ylim, name=name)
		pushViewport(vp0)
		pushViewport(vp1)
		if(box)
			grid.rect()
		if(axes) {
			grid.xaxis(gp=gpar(..., fontsize=fontsize))
			grid.yaxis(gp=gpar(..., fontsize=fontsize))
		}
		grid.text(ylab, x=unit(if(axes) -3 else -0.5,"lines"), rot=rot, gp=gpar(..., fontsize=fontsize))
	}
	else {
		seekViewport(name)
	}
	grid.lines(x=t, y=ts, default.units = "native", gp=gpar(...))
	upViewport(2)
}

