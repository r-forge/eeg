print.mean.bvadata <-
function(x, ..., digits = NULL, quote = FALSE, right = TRUE,
                 row.names = TRUE)
{
	fnames = strsplit(x$file,"\\+")[[1]]
	cat("file: ", paste("\n", fnames),"\n");
	print(c(nsamp=x$nsamp, nchan=x$nchan, ntrial=x$ntrial))
	invisible(x)
}

