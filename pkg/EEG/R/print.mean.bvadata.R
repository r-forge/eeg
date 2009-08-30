print.mean.bvadata <-
function(x, ...)
{
	fnames = strsplit(x$file,"\\+")[[1]]
	cat("file: ", paste("\n", fnames),"\n");
	print(c(nsamp=x$nsamp, nchan=x$nchan, ntrial=x$ntrial))
	invisible(x)
}

