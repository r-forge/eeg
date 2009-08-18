print.mean.bvadata <-
function(x, ...)
{
	cat("file: ", paste("\n",x$file,"\n"));
	print(c(nsamp=x$nsamp, nchan=x$nchan, ntrial=x$ntrial))
	invisible(x)
}

