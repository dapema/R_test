## The following two functions cache an the inverse of a matrix. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(mat = numeric()) {
  inv <- NULL
  set <- function(y) {
    mat <<- y
    inv <<- NULL
  }
  get <- function() mat
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix"
#returned by makeCacheMatrix above.
#If the inverse has already been calculated 
#(and the matrix has not changed), 
#then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(mat, ...){
  inv <- mat$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- mat$get()
  inv <- solve(data, ...)
  mat$setinverse(inv)
  inv
  
}
