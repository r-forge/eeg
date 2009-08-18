all.equal.bvadata <-
function(x,y,important, ...){
	if(missing(important))
		important = c('DataType','NumberOfChannels','SamplingInterval','SegmentDataPoints')
	isTRUE(all.equal(x$header[[1]][important],y$header[[1]][important])) && 
	   isTRUE(all.equal(x$header[-1], y$header[-1]))
}

