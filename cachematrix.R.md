makeCacheMatrix <- function(x = matrix()) {
            l <- NULL
            set <- function(y) {
                    x <<- y
                    l <<- NULL
            }
            get <- function() x
            setInverse <- function(Inverse) l <<- Inverse
            getInverse <- function() l
            list(set = set, get = get,
                 setInverse = setInverse,
                 getInverse = getInverse)
    }
    cacheSolve <- function(x, ...) {
            l <- x$getInverse()
            if(!is.null(l)) {
                    message("getting cached data")
                    return(l)
            }
            mat <- x$get()
            l <- solve(mat, ...)
            x$setInverse(l)
            l
    }

