`$.mbvadata.frame` <-
function(x, name)
{
	x = unclass(x)
	idx = pmatch(name, colnames(x$df), 0)
	if(idx==0) 
		NULL 
	else if(colnames(x$df)[idx]==x$data.nm) 
		x$data 
	else x$df[,idx]
}

