`/.mean.bvadata` <-
function(a,b)
{
	if(class(b)=="mean.bvadata"){
		tmp = a; a = b; b = tmp;
	}
	if(!is.numeric(b))
		stop('one of a and b should be numeric')
	a * (1/b)
}

