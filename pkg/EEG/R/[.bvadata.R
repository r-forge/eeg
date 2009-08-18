`[.bvadata` <-
function(x, sample=NA,channel=NA,trial=NA)
{
   inf = x$info()
   if(missing(sample))
       sample = 1:inf$nsamp;
   if(missing(channel))
       channel = 1:inf$nchan;
   if(missing(trial))
       trial = 1:inf$ntrial;
   dimnms = dimnames(x$x())
   if(!all(sample %in% dimnms[[1]] || abs(sample) %in% c(seq(dimnms[[1]]),0)) ||
      !all(channel %in% dimnms[[2]] || abs(channel) %in% c(seq(dimnms[[2]]),0)) ||
      !all(trial %in% dimnms[[3]] || abs(trial) %in% c(seq(dimnms[[3]]),0))
    )
       stop("subscript out of bounds")
   file = x$file
   header = x$header
   orig = y = x
   x = x$x()[sample, channel, trial, drop=FALSE];
   y$x=function() x;
   y$acrossTrials = function(func,samples=1:dim(x)[1], channels=1:dim(x)[2], trials = 1:dim(x)[3], ...)
          apply(x[samples,channels,trials, drop=FALSE],c(1,2), func, ...);
   y$mean = function(...) y$acrossTrials (mean, ...); 
   y$sd = function(...) y$acrossTrials (sd, ...); 
   y$se = function(..., trials=1:dim(x)[3]) y$acrossTrials (sd, ..., trials=trials)/sqrt(dim(x)[3]);
   y$info = function() list(nsamp = dim(x)[1], nchan=dim(x)[2], ntrial=dim(x)[3]);
   y$file= file
   y$markers = bvaMrkTrialSel(y$markers, trial)
   y$header = header;
   y$header$`Common Infos`$NumberOfChannels = dim(x)[2]
   y$header$`Common Infos`$DataPoints = dim(x)[1] * dim(x)[3]
   y$header$`Common Infos`$SegmentDataPoints = dim(x)[1]
   y$header$`Channel Infos` = y$header$`Channel Infos`[channel]
   y$header$Coordinates = y$header$Coordinates[channel]
   if(!is.null(y$ntrial))
   		y$ntrial = y$ntrial[trial]
   class(y) = class(orig);
   orig = NULL
   y
}

