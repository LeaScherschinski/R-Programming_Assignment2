## Caching the Inverse of a Matrix

## The following function "makeCacheMatrix" creates a special matrix, whose objects 
## are cached inversely. It is a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        invrs <- NULL
        set <- function(y){
          x <<- y
          invrs <<- NULL
        }
                get <- function() x
                setinverse <- function(inverse) invrs <<- inverse
                getinverse <- function() invrs
                list(set = set,
                     get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
}


## The following function "cacheSolve" gives us the inverse of the special matrix
## returned by the above function "makeCacheMatrix". If the inverse has already
## been calculated (and the matrix has not changed), then this function should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        invrs <- x$getinverse()
        if (!is.null(invrs)) {
              message("getting cached data")
          return(invrs)
        }
        data <- x$get()
        invrs <- solve(data,...)
        x$setinverse(invrs)
        invrs
}
