makeCacheMatrix <- function(x = matrix()) 
{
	m <- NULL
	set <- function(y)
	{
		x <<- y
		m <<- NULL
	}

	get <- function() x
	setmatrix <- function(solve) m <<- solve
	getmatrix <- function() m

	# create list of 4 functions
	list(set=set,get=get,setmatrix=setmatrix,getmatrix=getmatrix)
}

# computes the inverse unless its in already computed and cached -
# in that case retrieves and returns the cached matrix
cacheSolve <- function(x=matrix(), ...) 
{
    m<-x$getmatrix()

    # return matrix if its cached
    if(!is.null(m))
    {
      message("getting cached data")
      return(m)
    }
    m<-solve(x$get())
    x$setmatrix(m)
    m
}
