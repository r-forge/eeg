getInfo <-
function(x,what){if(length(x)==1) NULL else if(what %in% names(x)) x[[what]] else do.call("c",sapply(x,getInfo,what=what))}

