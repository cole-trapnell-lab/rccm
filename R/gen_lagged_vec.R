#' @title Generate lagged vectors
#' @description Generates lagged vectors and combines them into a shadow manifold
#' @param X a chronological series of observations
#' @param tau the lag a which to generate each lagging vector
#' @param E the number of lags to include
#' @return list
#' @keywords weights
#' @seealso \code{\link{get_nn}} 
#' @examples
#' #str_length(letters)
#' #str_length(c("i", "like", "programming", NA))
gen_lagged_vec <-
function(X, tau=1, E=3)
{
    L <- length(X)
    x <- array(dim=c(L-(E-1)*tau,E)) # only E columns (embeddin dimension)
    tmap <- (1+(E-1)*tau):L # smallest time index to largest time index 
    for(i in tmap)
        x[i-((E-1)*tau),] <- X[seq(i,(i-(E-1)*tau),-tau)] # the time direction is reversed 

    return(list(x=x,tmap=tmap))
}
