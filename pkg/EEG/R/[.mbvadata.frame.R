`[.mbvadata.frame` <-
function(x, rows, cols)
{
	x = unclass(x)
	if(missing(rows))
		rows = 1:nrow(x$df)
	if(missing(cols))
		cols = 1:ncol(x$df)
	if(is.character(rows))
		rows = pmatch(rows, rownames(x$df))
	if(is.character(cols))
		cols = pmatch(cols, colnames(x$df))
	df = x$df[rows, cols, drop=FALSE]
	mbvadata = x$data[rows]
	rownames(df) = names(mbvadata)
	if(x$data.nm %in% colnames(x$df)[cols]) 
		structure(list(df=df, data.nm = x$data.nm, data=mbvadata), class="mbvadata.frame")
	else
		df
}

