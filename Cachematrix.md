#Programming Assignment2

## makeCacheMatrix & cacheSolve funtions work together to compute the inverse
## of a matrix and caches the result for future and faster computation.


## makeCacheMatrix takes a matrix as its input. It returns a list of objects comprising
## of function values of set, get, setinv, and getinv. Here, Set sets the value of x,
## get retrieves the value of x, setinv sets the value for the inverse of x and
## getinv retrieves the value of the inverse of x.

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


## cacheSolve reads the list returned by makeCacheMatrix & checks the necessary objects
## to see if the inverse has already been calculated. In case it exists, the function returns an output as 
## message notifying the user that the value has been cached previously and is being
## retrieved. In case doesn't exist, the function calculates the inverse and then caches it for 
## future use.

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
