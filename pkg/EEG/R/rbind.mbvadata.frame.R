rbind.mbvadata.frame <-
function(a, b)
{
	if(class(a)!=class(b))
		stop("can't bind mbvadata.frame with different structure")
	a = unclass(a)
	b = unclass(b)
	df = rbind(a$df, b$df)
	mbvadata = c(a$data, b$data)
	if(!is.null(dim(df)))
		rownames(df) = names(mbvadata)
	structure(list(df=df, data.nm=a$data.nm, data=mbvadata), class="mbvadata.frame")
}

