manova.reshape <-
function (data, idvar, factors, v.names) 
{
    data
    if (missing(v.names)) 
        v.names = setdiff(colnames(data), c(idvar, factors))
    data$`hopefuly unique` = do.call(paste, c(data[factors], 
        list(sep = ".")))
    reshape(data[, c(idvar, v.names, "hopefuly unique")], idvar = idvar, 
        timevar = "hopefuly unique", v.names = v.names, direction = "wide")
}

