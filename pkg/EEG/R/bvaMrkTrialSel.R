bvaMrkTrialSel <-
function(mrk, trials){
	if(!is.data.frame(mrk)||NROW(mrk)==0){
		warning('empty markers data.frame')
		return(mrk)
	}
	strt = which(mrk$type == 'New Segment')
	endt = c(strt[-1]-1,NROW(mrk)) 
	rows = mapply(seq, strt, endt)
	mrk[unlist(rows[trials]),]
}

