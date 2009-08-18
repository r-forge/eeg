timeindx <-
function(x, start, end){dt=getInfo(x,'SamplingInterval')/1e3;shft=x$time0indx[1]-x$segmindx[1];time=(1:x$info()$nsamp-1-shft)*dt; seq(time)[time>=start & time<end]}

