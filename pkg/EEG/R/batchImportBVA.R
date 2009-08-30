batchImportBVA <- 
function(path, pat = "vhdr$", nm.pat="^(.+).+vhdr", newnm.pat){
	nms = dir(path, pat)
	enms = dir(path, pat, full.names=TRUE)
	nc = length(strsplit(nm.pat, "\\([^\\)]+\\)",perl=TRUE)[[1]]) - 1 # number of components for new names
	if(missing(newnm.pat))
		newnm.pat = paste(paste("\\",1:nc,sep=""),collapse=".")
	nm.repl = newnm.pat
	nms = gsub(nm.pat,nm.repl,tolower(dir(path,pat)),perl=TRUE)
	names(enms) = nms
	cat(length(enms)," data sets found.\n")
	if(length(enms)>0)
		cat("Importing...","\n")
	lapply(enms, function(f){
		i = which(f==enms)[1]
		cat(f," (", round(100*i/length(enms)), "%)\n"); 
		flush.console(); 
		try(importBVA(f)); 
	})
}

