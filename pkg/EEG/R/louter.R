louter <-
function(x, y, FUN, ...) {if(missing(y)) y=x else if(is.function(y)){FUN = y; y=x}; sapply(x, function(x) sapply(y, function(y) FUN(x,y,...)))} # outer doesn't work well with lists

