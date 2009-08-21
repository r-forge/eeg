## names2factors - takes a vector of (e.g. file-)names that uses a fixed scheme to 
## encode a factorial design and returns a data.frame with the factors decoded
## This is useful for filenames which encode specific data
##
## @nams 		a vector containing the names
## @formula 	a formula that specifies how to decode the names
## @sep			a character that is used as a seperator in the names
## result is a data frame with the decoded factors 
#  example:
#    nams = c( "pp01_me_dlf_catch_correct_dat","pp01_me_dlf_ldd_correct_dat","pp01_me_dlf_ldd_error_dat")
#    names2factors(nams,  ~ subj+initials+task+stim:level+resp+ext, sep="_")
#
#                               subj initials task stim level    resp ext
# pp01_me_dlf_catch_correct_dat pp01       me  dlf  cat    ch correct dat
# pp01_me_dlf_ldd_correct_dat   pp01       me  dlf   ld     d correct dat
# pp01_me_dlf_ldd_error_dat     pp01       me  dlf   ld     d   error dat

names2factors <-
function(nams, formula, sep=".", ...){
	v = all.vars(formula);
	t = terms(formula)
	lab = attr(t, 'term.labels')
	ord = attr(t, 'order');
	tmp = lapply(v, function(x) grep(paste("\\b",x,"\\b",sep=""),lab))
	vgrps = unique(unlist(lapply(tmp, function(i) if(length(i)>0) lab[max(i)])))
	esc.sep = paste("\\",sep,sep="")
	nms = strsplit(nams, esc.sep)
	if(!all(sapply(nms,length) == length(vgrps)))		stop("names do not break according to formula with sep = '", sep, "'")
	nvgrps = sapply(strsplit(vgrps, ":"), length)
	r = sapply(nms, function(nm){
			n = nchar(nm)
			 l1 = ceiling(n/nvgrps)  # label length of first few factors
			 l2 = l1*nvgrps - n              # label length for last factor
			nl1 = (n-l2) %/% l1            # number of occurences of length l1 factor labels
			nl2 = (l2!=0) + 0              # number of occurences of length l2 factor label
			if(any(l1<0) | any(l2<0))
				stop("a term in the formula specifies too many factors for the number of letters available") 
			tmp = mapply(function(a,b,c,d) list(c(rep(b,a),rep(d,c))),nl1, l1, nl2, l2)
			patt = sapply(tmp, function(x){paste("(.{",x,"})",sep="",collapse="")})
			repl = sapply(tmp, function(x){paste("\\",1:length(x),sep="", collapse=sep)})
			res=unlist(strsplit(mapply(gsub, patt, repl, nm), esc.sep))
#			names(res)=NULL
			res
		})
	on.exit(r)
	if(is.matrix(r)){
		rownames(r) = v;
		colnames(r) = nams
		data.frame(t(r));
	} else r
}



