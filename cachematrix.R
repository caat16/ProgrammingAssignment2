##Create matrix object and cache his inverse.
makeCacheMatrix <- function(x = matrix()){
        inv <- NULL
        set <- function(y) {
              x <<- y
              inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse 
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
#Compute the inverse of the matrix with makeCacheMatrix

#Second part: Cache solve to return a matrix will be inversed of x.
cacheSolve <- function (x, ...){
      inv <- x$getInverse()
      if(!is.null(inv)){
          message("getting cached data")
        return (inv)
      }
      mat <- x$get()
      inv <- solve(mat)
      x$setInverse(inv)
}
