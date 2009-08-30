all.equal.bvadata <-
function(target, current,important, ...){
	if(missing(important))
		important = c('DataType','NumberOfChannels','SamplingInterval','SegmentDataPoints')
	isTRUE(all.equal(target$header[[1]][important],current$header[[1]][important])) && 
	   isTRUE(all.equal(target$header[-1], current$header[-1]))
}

