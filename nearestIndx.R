#returns the nearest occurence of x in vec
nearestIndx <- function(x, vec)
{
    smallCandidate <- findInterval(x, vec, all.inside=TRUE)
    largeCandidate <- smallCandidate + 1
    #nudge is TRUE if large candidate is nearer, FALSE otherwise
    nudge <- 2 * x > vec[smallCandidate] + vec[largeCandidate]
    return(smallCandidate + nudge)
}

