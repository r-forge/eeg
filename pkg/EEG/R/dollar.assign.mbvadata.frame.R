`$<-.mbvadata.frame` <-
function(x, name, value)
{
	x = unclass(x)
	idx = pmatch(name, colnames(x$df),0)
	if(colnames(x$df)[idx]==x$data.nm){
		x[["data"]] = value
		names(x[["data"]]) = names(value)
		x$df[,idx] = names(value)
	}
	else x$df[,idx] = value
	structure(x, class="mbvadata.frame")
}

