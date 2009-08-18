plot.bvadata <-
function(x, ..., main=deparse(substitute(x))) {
	plot(as.ts(x, ...), ..., main=main);
}

