hotellings <-
function(data,mu=rep(0,NCOL(data)),print=TRUE){
   cv  <- cov(data);
   mn  <- apply(data,2,mean);
   n   <- nrow(data);
   p   <- ncol(data)
   df1 <- p
   df2 <- n-p
 
   T2  <- (n / (n-1)) * t(mn-mu) %*% solve(cv) %*% (mn-mu);
   F   <- T2 * df2/df1;
 
   prb <- 1-pf(F,df1,df2);
   dd  <- t(data) - mu %o% rep(1,n)
   phoc  <- apply( dd, 1, t.test );
   ph.p  <- unlist(lapply(phoc,function(x) x$p.value));
   ph.t  <- unlist(lapply(phoc,function(x) x$statistic));
   ph.df <- unlist(lapply(phoc,function(x) x$parameter));
 
   if(print) cat('\nOmnibus test for null hypothesis that the mean vector = [',mu,']\n\n');

   T2 <- c('Hotellings T2 '=T2)
   if(print) print(data.frame(Value=T2, 'Exact F'=F, Hyp.df=df1, Err.df=df2, Signif.=prb));
   if(print) cat('\nPost hoc t-tests\n');

   names(ph.t) <- names(data);
   r<-data.frame(t.value=ph.t,df=ph.df,Sig.=round(ph.p,9));
   if(print) print(r)
   if(print) cat('\n');
 
   invisible(list(Hotelings.T2=T2, Exact.F=F, Hyp.df=df1, Err.df=df2, Signif.=prb, posthoc=r));
 }

