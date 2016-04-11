## This R file cachematrix. R contains two functions
## The functions are very similar to the example functions on Vectors
## makeCacheMatrix builds op a list of 4 functions on a Matrix


makeCacheMatrix <- function(x = matrix()) {
    elc <- NULL
    set <- function(y) {
      x <<- y
      elc <<- NULL
    }
    get <- function() x
    setreverse<- function(reverse) elc <<-reverse
    getreverse <- function() elc
    list(set = set, get = get,
         setreverse = setreverse,
         getreverse = getreverse)
    
  }
  

## Function cacheSolve equate the inverse if it is not already done.
## if the inverse was already calculated, it simply returns it from cache
## Function solve calculates the inverse of a matrix
## with the use of the super operand <<- variables are stored in an environment outside the function

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
  elc <- x$getreverse()
  if(!is.null(elc)) {
    message("getting cached data")
    return(elc)
  }
  data <- x$get()
  elc <- solve(data, ...)
  x$setreverse (elc)
  elc
  
  }
