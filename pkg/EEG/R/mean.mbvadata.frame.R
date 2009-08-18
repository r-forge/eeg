mean.mbvadata.frame <-
function(x, ...)
{
	x = unclass(x)
	data = x$data
	z = data[[1]]
	for(i in 2:length(data)) z = z + data[[i]]
	z
}

