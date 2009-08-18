readMarkerFile <-
function(file, what=3, keep.names = FALSE)
{
   mrk = readLines(file)
   mrk = mrk[-(1:grep("\\[Marker Infos\\]",mrk))]
   mrk = mrk[-grep("^;",mrk)]
   mrk
   mrkans = t(sapply(mrk, function(x){y=strsplit(x, split="[,=]")[[1]];y[7]=y[7];y})) 
   if(!keep.names) {
     rownames(mrkans) = mrkans[,1];
     mrkans = mrkans[,-1];
     colnames(mrkans) = c("type", "description", "position", "size", "channel", "date")
   }
   mrkans
}

