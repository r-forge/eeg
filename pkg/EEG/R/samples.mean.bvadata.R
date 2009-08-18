samples.mean.bvadata <-
function(x, times, from=start(x$mean), to=end(x$mean))
{ # returns the sample indices of the times in times or between from and to
	if(missing(times))
		if(missing(to))
			which(time(x$mean)>from)[1]
		else
			which(time(x$mean)>from & time(x$mean)<to)
	else
		apply(outer(time(x$mean),times,">="), 2, which.max )
}

