## This function takes a square matrix and generates the inverse of it by cache or solving for the 
##inverse if the matrix inverse has not been generated
## 

## Function that caches a square matrix and generates it's inverse if getInverse is selected 


makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setSolve = setSolve,
         getInverse = getInverse)
  }


## Looks for the cache matrix and generates it's inverse if one has not been done, 
## otherwise it just gets the cached matrix

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  mat <- x$get()
  m <-solve(mat, ...)
  x$setSolve(m)
  m
}


