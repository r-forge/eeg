as.ts.bvadata <-
function(x, ...) {
	dt=getInfo(x, 'SamplingInterval')/1e3; # msec
	ts(x$mean(...), start=-(x$time0indx[1]-x$segmindx[1])*dt,deltat=dt); 
}

