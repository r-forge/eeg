print.bvadata <-
function(x, ...) {
   cat("file: ", paste("\n",x$file),"\n");
   print(unlist(x$info()))
}

