## some function for doing the fertilty period/cohort exercises

## get_hfd_fertility_matrix

get_hfd_fertility_matrix <- function(file, code)
{
    library(data.table)
    ##     dt <- fread("~/Downloads/Files-4/zip_w/asfrRR.txt")
    dt <- fread(file)
    ##     dt <- dt[Code == "USA"] ## keep just USA
    dt <- dt[Code == code]
    dt[, x := as.numeric(Age)] ## assign age variable as numeric "x"
    dt[Age == "12-", x := 12] ## fix youngest and oldest ages
    dt[Age == "55+", x := 55]
    A = dt[, xtabs(ASFR ~ x + Year)]
    return(A)
}

## table_mean

table_mean <- function(Fx, x.mid)
{
    mu = sum(x.mid * Fx)/sum(Fx)
    return(mu)
}


get.moment <- function(fx, n.moment, x = NULL, n = NULL)
{
    ## compute moment from density fx
    ##    sum(((x+n/2)^n.moment)*fx)/sum(fx)

    ## usage:
    ## set.seed = 1
    ## y = rnorm(1000000, mean = 100, sd = 10)
    ## fy = table(floor(y))
    ## my.x2.bar = get.moment(fy, 2)


    ## assign age x if not given
    if (is.null(x))
    {
        ## if names, use
        if(!is.null(names(fx)))
            x = as.numeric(names(fx))
        ## if no names,  default 0, 1, ..
        if(is.null(names(fx)))
            x = seq(fx) - 1
    }
    ## assume open interval is same length as next to last
    if(is.null(n))
	n = c(diff(x), diff(x)[length(diff(x))])


    moment = sum(((x+n/2)^n.moment)*fx)/sum(fx)
    return(moment)
}

get.mean <- function(fx, x = NULL, n = NULL)
{
    get.moment(fx, n.moment = 1, x, n)
}
get.var <- function(fx, x = NULL, n = NULL)
{
    x.bar =  get.mean(fx, x, n)
    x2.bar = get.moment(fx, n.moment = 2, x, n)
    s2 = x2.bar - x.bar^2
    return(s2)
}
get.sd <- function(fx, x = NULL, n = NULL)
{
    s2 = get.var(fx, x = NULL, n = NULL)
    s = sqrt(s2)
    return(s)
}


## center_diff
center.diff <- function(x, end.fill = F)
{
    ## approximate derivatives with discrete data by taking central
    ## differences d(x) = ([f(x+1) - f(x)] + [f(x) - f(x-1)])/2
    ##                  = [f(x+1) - f(x-1)]/2
    ## if end.fill = T, then first and last differences are not centered.

    ## useful for Bongaarts-Feeney, Gompertz fertility, and other
    ## fitting of models that are developed in continuous time and
    ## involve derivatives

    forward.diff = c(diff(x), NA)
    backward.diff = c(NA, diff(x))
    out = (forward.diff + backward.diff)/2
    if(end.fill)
    {
        out[1] = diff(x)[1]
        out[length(out)] = diff(x)[length(diff(x))]
    }
    ## preserve names if exist
    if(!is.null(names(x)))
        names(out) = names(x)
    return(out)
}


