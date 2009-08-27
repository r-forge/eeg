`*.mean.bvadata` <-
function(a,b)
{
	if(class(b)=="mean.bvadata"){
		tmp = a; a = b; b = tmp;
	}
	if(!is.numeric(b))
		stop('one of a and b should be numeric')
	a$sum.x[] = b[] * a$sum.x[]
	a$sum.x2[] = (b[])^2 * a$sum.x2[]
	a$mean[] = b[]*a$mean[]
	a$sd[] = b[]*a$sd[]
	a
}

`\.mean.bvadata` <-
function(a,b)
{
	if(class(b)=="mean.bvadata"){
		tmp = a; a = b; b = tmp;
	}
	if(!is.numeric(b))
		stop('one of a and b should be numeric')
	a * (1/b)
}