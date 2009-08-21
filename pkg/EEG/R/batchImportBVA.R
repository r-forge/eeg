batchImportBVA <- 
function(path, pat = "\\.vhdr$",
		nm.pat="^(.).(\\d{2}).{4}([a-z]{2,3}).([a-z]{3}).([a-z]).+vhdr",
		newnm.pat=paste(paste("\\",1:nc,sep=""),collapse=".")){
	nms = dir(path, pat)
	enms = dir(path, pat, full.names=TRUE)
	nc = length(strsplit(nm.pat, "\\([^\\)]+\\)")[[1]]) # number of components for new names
	nm.repl = newnm.pat
	nms = gsub(nm.pat,nm.repl,tolower(dir(path,pat)),perl=TRUE)
	names(enms) = nms
	cat(length(enms)," data sets found.\n")
	if(length(enms)>0)
		cat("Importing...","\n")
	lapply(enms, function(f){cat(f,"\n"); flush.console(); importBVA(f); })
}

