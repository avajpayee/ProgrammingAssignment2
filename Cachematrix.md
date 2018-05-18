#Programming Assignment2

makeCacheMatrix <- function(x = matrix())
{
       
       invm <- NULL
       set <- function(y) 
       {
		x <<- y
        	invm <<- NULL
       }
        get <- function() x
        setInverse <- function(inverse) invm <<- inverse
        getInverse <- function() invm
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


cacheSolve <- function(x, ...) 
{
        
        invm <- x$getInverse()
        if (!is.null(invm)) 
        {
        	message("getting cached data")
        	return(invm)
        }
        data <- x$get()
        invm <- solve(data, ...)
        x$setInverse(invm)
        invm
}
