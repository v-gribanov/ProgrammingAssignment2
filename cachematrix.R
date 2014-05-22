# Below there are 2 functions that implement the calculation of   
# inverse matrix. For large matrix the computation of
# its inverse one is time-consuming operation. 
# The idea is to cache the inverse matrix so that when 
# we need it again, it can be looked up in the cache 
# rather than recomputed



# The function implements a general interface to get access to the
# matrix and its inverse matrix. 
# It contains 4 features implemented:
# get - to get matrix
# set - to set matrix
# getinverse - to get inverse matrix
# setinverse - to set inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inv <<- inv
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



# The function return the inverse matrix for given matrix.
# It first checks to see if the inverse matrix
# has already been calculated. If so, it gets the inverse matrix
# from the cache and skips the computation. Otherwise, 
# it calculates the inverse matrix and sets the value 
# of the inverse matrix in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  inv <- solve(x$get())
  x$setinverse(inv)
  inv
}
