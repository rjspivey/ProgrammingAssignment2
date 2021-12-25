## The program inputs a matrix, creates a space for the matrix solution, looks for the matrix solution space, and calculates the matrix's inverse if no solution exists

## makeCacheMatrix creates the space for calculating the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
          m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## cacheSolve calculates the matrix's inverse if no inverse currently exist.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
