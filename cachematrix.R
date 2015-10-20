## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix - Creates a Matrix object that can cache its inverse
  # fn set(y) ->  receive a new Matrix and set the cached inverse to NULL
  # fn get() -> get the original Matrix
  # fn setinverse(inverse) -> store the inverse of the Matrix in the cache
  # fn getinverse() -> get the cached matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# cacheSolve - Computes the inverse of the matrix returned by makeCacheMatrix, should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
