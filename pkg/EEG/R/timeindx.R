timeindx <-
function(x, start, end) UseMethod("timeindx")

timeindx.bvadata <-
function (x, start, end) 
{
    dt = getInfo(x, "SamplingInterval")/1000
    shft = x$time0indx[1] - x$segmindx[1]
    time = (1:x$info()$nsamp - 1 - shft) * dt
    seq(time)[time >= start & time < end]
}

timeindx.mean.bvadata <-
function (x, start, end) 
{
    dt = x$header$`Common Infos`$SamplingInterval/1000 # microseconds to milliseconds
    shft = x$time0indx[1] - x$segmindx[1]
    time = (1:x$nsamp - 1 - shft) * dt
    seq(time)[time >= start & time < end]
}

