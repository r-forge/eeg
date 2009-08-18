compERPplot <-
function(x,y,i=2,cicolx="gray", cicoly="steelblue", xdens=NULL, ydens=12,
xlocleg=max(timex)*.7, ylocleg=ylim[2]*.9)
{
 xm = x$mean()[,i]; xse = x$se()[,i]; ym = y$mean()[,i]; yse = y$se()[,i];
 xuci = xm+1.96*xse; xbci = xm-1.96*xse; yuci=ym+1.96*yse; ybci=ym-1.96*yse;
 ylim = range(xuci,xbci,yuci,ybci)
 timex = (x$header$`Common Infos`$SamplingInterval/1e3)*(-1+1:length(xm)); # msec
 timey = (y$header$`Common Infos`$SamplingInterval/1e3)*(-1+1:length(ym)); # msec
 plot(timex,xm, type='l', col=2, lwd=3, ylim=ylim,xlab="Time",ylab=expression(Amplitude~(mu~V)))
 polygon(c(timex,rev(timex)), c(xuci,rev(xbci)), col=cicolx, density=xdens);
 polygon(c(timey,rev(timey)), c(yuci,rev(ybci)), col=cicoly, density=ydens,angle=365-45);
 lines(timex, xm, col=2, lty=1, lwd=3)
 lines(timey, ym, col=3, lty=1, lwd=3)
 legend(xlocleg,ylocleg, c(deparse(substitute(x)),deparse(substitute(y))), fill=c(cicolx,cicoly), col=2:3, lwd=3, lty=1, density=c(xdens,ydens))
}

