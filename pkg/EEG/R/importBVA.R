importBVA <-
function(file=file.choose())
{
   txt = scan(file,character(), sep="\n", quiet=TRUE);
   txt = txt[-grep("^\\s*;",txt)]; # remove comments
   if(length(grep("Brain Vision Data Exchange Header File", txt))==0)
     stop("not a valid Brain Vision Data Exchange Header File");

   sects = grep("^\\[(\\w| )+\\]$", txt);
   str = list()
   length(str) = length(sects)
   names(str) = gsub("\\[|\\]","",txt[sects]);
   # parse header info into lists
   for(i in 1:(length(str)-1)){
      tmp = txt[sects[i]:(sects[i+1]-2)+1];
      str[[i]] = eval(parse(text=paste("list(",paste(gsub("=([^0-9]|.*[^0-9\\.].*)$",'="\\1"',tmp),collapse=","),")")));
   }
   tmp = txt[(sects[length(sects)]+1):length(txt)];
   str[[length(sects)]] = eval(parse(text=paste("list(",paste(gsub("=(.*[^0-9\\.].*)$",'="\\1"',tmp),collapse=","),")")));
  
   dfile = gsub("[^\\\\]+$",str$`Common Infos`$DataFile,file);
   mfile = gsub("[^\\\\]+$",str$`Common Infos`$MarkerFile,file);
   if(is.null(dfile))
      warning("no data file specified in header")
   else
      dfile = gsub("[^/]+?\\.vhdr", dfile, file)
   if(is.null(mfile))
      warning("no marker file specified in header")
   else
      mfile = gsub("[^/]+?\\.vhdr", mfile, file)
   x = readBVADataFile(dfile,mfile,dec=str$`ASCII Infos`$DecimalSymbol);
   x$header = str;
   x
}

