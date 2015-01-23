## The functions serve the purpose of calculating the inverse of a matrix,
## with an optimization that involves storing the calculated inverse in a cache.
## Once calculated, the inverse stored in the cache can be reused until the matrix is changed.

## This function creates a special object containing 4 functions.
## The first 2 functions help to set and get the matrix, while the next 2 help in setting and getting its inverse.
makeCacheMatrix <- function(x = matrix()) {
	## Initialize the cached inverse value to NULL
	cachedInverse <- NULL

	## Sets the value of the matrix
	## Also, invalidate the cache by setting it to NULL
	## This will force a recalculate of the inverse the next time it is needed
	set <- function(y) {
		x <<- y
		cachedInverse <<- NULL
	} 
	
	## Retrieves the value of the matrix
	get <- function() x
	
	## Sets the value of the inverse
	setInverse <- function(inverseMatrix) cachedInverse <<- inverseMatrix

	## Retrieves the value of the inverse
	getInverse <- function() cachedInverse 

	## Returns a list of the functions in this object
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the "special" matrix created above.
## The function takes the "matrix" object as input. If the inverse has already been computed and cached,
## then it returns the value from the cache. A message has been added to easily know when this is happening.
## Otherwise, the method will compute the inverse, save it in the cache and then return the inverse.

cacheSolve <- function(x, ...) {
        
	## Gets the cached value of the inverse
	inverse <- x$getInverse()

	## If the inverse has already been cached, then return from the cache
	if (!is.null(inverse)) {
		message("getting data from cache")
		return (inverse)
	}

	## If the inverse has not been cached, then get the matrix and calculate the inverse
	data <- x$get()
	inverse <- solve(data, ...)

	## Save the inverse in the cache
	x$setInverse(inverse)

	## Return the value of the inverse
	inverse
}
