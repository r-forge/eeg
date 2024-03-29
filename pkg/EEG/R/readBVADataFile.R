readBVADataFile <-
function(file=file.choose(),mfile, dec=".")
{
	if (missing(mfile))
	    mfile <- gsub("\\..+$",".VMRK",file)
   x        <- as.matrix(read.table(file,TRUE),dec=dec);
   mrk      <- as.data.frame(readMarkerFile(mfile))
   segmRows <- stimRows <- mrk$type=='New Segment'
   segindx  <- tindx <- as.numeric(as.character(mrk$position[stimRows]))
   if(any(diff(diff(tindx))!=0)){
       # check if this is due to non-trial start Stimulus markers; guess which to use
       typeNum = as.numeric(gsub("\\D","",mrk$description,perl=TRUE))
       typeTbl = table(na.omit(typeNum[stimRows]))
       guessidx = which.max(typeTbl)
       guess <- as.numeric(names(guessidx))
       if(length(typeTbl)>1)
          warning("More than one type of stimulus marker. Guessed \"",mrk$description[stimRows & typeNum==guess][1],"\"\n to be the relevant marker (certainty = ",typeTbl[guessidx]/sum(typeTbl),")\n")
       tindx <- as.numeric(as.character(mrk$position[stimRows & typeNum==guess]))
       triallen = diff(tindx)[1]
       if(any(diff(diff(tindx))!=0)){
         stop("FATAL -- Not all trials have same number of samples.");
         triallen = NA;
       }
   } else
       triallen = diff(tindx)[1]
   dm = dim(x)
   nchan = dm[2]
   nsamp = dm[1]
   channames = colnames(x)   
   ntrial = length(tindx)
   trialnms = paste('trial',1:ntrial,sep='.')
   dim(x) = c(triallen, ntrial, nchan)
   x = aperm(x, c(1,3,2))
   sampnme = paste("t",1:triallen,sep="")
   dimnames(x) = list(sampnme, channames, trialnms)
   attr(x,"markers") = mrk
   stimRows <- mrk$type=='Stimulus'
   tindx <- as.numeric(as.character(mrk$position[stimRows]))
   tim0Rows <- mrk$type=='Time 0'
   tim0indx <- as.numeric(as.character(mrk$position[tim0Rows]))
   info = list(tindx = tindx, trial_len = triallen, ntrial=ntrial, nsamp=nsamp, nchan=nchan)
   y = list(x=function() x,
     acrossTrials = function(func,samples=1:dim(x)[1], channels=1:dim(x)[2], trials = 1:dim(x)[3])
          apply(x[samples,channels,trials, drop=FALSE],c(1,2), func),
     mean = function(...) y$acrossTrials (mean, ...),
     sd = function(...) y$acrossTrials (sd, ...),
     se = function(..., trials=1:dim(x)[3]) y$acrossTrials (sd, ..., trials=trials)/sqrt(dim(x)[3]),
     info = function() list(nsamp = dim(x)[1], nchan=dim(x)[2], ntrial=dim(x)[3]),
     stimindx = tindx,
     segmindx = segindx,
     time0indx = tim0indx,
     file= file
   )
   y$markers = mrk
   class(y) = 'bvadata';
   y
}

