eeg.plot <-
function(obj, ..., xlim=NA, ylim=NA, add=FALSE, width=.10, height=width/2, fontsize=7, 
	label=TRUE, axes = FALSE, na.rm=TRUE)
{
	loc = do.call('rbind',lapply(obj$header$Coor, 
				function(x) eval(parse(text=paste('c(',x,')')))))
	loc[,2:3] = loc[,3:2] * 2 * pi / 360
	chinfo = do.call('rbind', strsplit(unlist(obj$header$`Channel Infos`),",") )
	# Cartesian coordinates:
	x = loc[,1]*cos(loc[,2])*sin(loc[,3])
	y = loc[,1]*sin(loc[,2])*sin(loc[,3])
	z = loc[,1]*cos(loc[,3])
	# my projection: ?
	x = ifelse(sqrt(x^2+y^2)>0, abs(loc[,3]) * x / sqrt(x^2+y^2) / max(abs(loc[,3])), 0);
	y = ifelse(sqrt(x^2+y^2)>0, abs(loc[,3]) * y / sqrt(x^2+y^2)^(1/10) / max(abs(loc[,3])), 0);
	x[loc[,1]==0] = c(-1,  1)
	y[loc[,1]==0] = c(-1, -1)
	if(!add){
		grid.newpage()
		pushViewport(vp0 <- dataViewport(range(x)+width,range(y),exten=.15, name="head"))
	}
	else {
		seekViewport("head")
		vp0 <- current.viewport()
	}
	for(i in 1:nrow(loc)){
		chnm = chinfo[i,1]; 
		unit = chinfo[i,4]
		ylab = if(chnm %in% label || label==TRUE) paste(chnm," (", unit,")",sep="") else ""
		grid.tsplotAt(as.ts(obj[,i,], na.rm=na.rm), x[i], y[i], width, height, ylab=ylab, fontsize=fontsize, 
			xlim=xlim, ylim=ylim, name = chinfo[i,1], add=add, axes = chnm %in% axes || axes==TRUE, ...)
	}
	upViewport()
	invisible(vp0)
}

