`[.mean.bvadata` <-
function(x, samp=1:x$nsamp, chan=1:x$nchan, ...)
{
	x$sum.x = x$sum.x[samp, chan, drop=FALSE]
	x$sum.x2 = x$sum.x2[samp, chan, drop=FALSE]
	x$mean = ts(x$mean[samp, chan, drop=FALSE], start=start(x$mean), deltat=deltat(x$mean))
	x$sd = ts(x$sd[samp, chan, drop=FALSE], start=start(x$sd), deltat=deltat(x$sd))
	x$n = x$n[samp, chan, drop=FALSE]
	x$nchan = length(chan)
	x$nsamp = length(samp)
	x$header$`Common Infos`$DataPoint = length(samp)
	x$header$`Common Infos`$NumberOfChannels = length(chan)
	chnm = dimnames(x$sum.x)[[2]]
	hchnm = unlist(x$header$`Channel Infos`)
	x$header$`Channel Infos` = x$header$`Channel Infos`[pmatch(chnm,hchnm)]
	x$header$`Coordinates` = x$header$`Coordinates`[pmatch(chnm,hchnm)]
	x
}

