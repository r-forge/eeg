mintersect <-
function(x,y,...){is=intersect(x,y); if(length(list(...))>0) mintersect(is,...) else is;}

