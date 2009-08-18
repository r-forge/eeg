mbvadata.frame <-
function(...)
{
	args = list(...)
	nms = names(args)
	nms[nms==""] = paste("V",1:length(args),sep="")[nms==""]
	names(args) = nms
	mbva.data = args[[1]]
	if(!all(sapply(mbva.data,class)=="mean.bvadata"))
		stop('first argument should be mean.bvadata list')
	arg1 = list(names(mbva.data)) 
	names(arg1) = names(args)[1]
	df = do.call("data.frame", c(arg1, args[-1]))
	row.names(df) = names(mbva.data)
	structure(list(df=df, data.nm = names(arg1)[1], data = mbva.data), class="mbvadata.frame")
}

