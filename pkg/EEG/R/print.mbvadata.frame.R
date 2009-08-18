print.mbvadata.frame <-
function(x, ...)
{
	print(unclass(x)$df, ...)
	invisible(x)
}

