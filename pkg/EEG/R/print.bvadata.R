print.bvadata <-
function(x, ..., digits = NULL, quote = FALSE, right = TRUE,
                 row.names = TRUE) {
   cat("file: ", paste("\n",x$file),"\n");
   print(unlist(x$info()))
}

