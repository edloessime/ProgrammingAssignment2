## Matrix inversion is computationally heavy and is often better of being cached as opposed
## being computed over and over again. The two functions below offer an option to do this activity.

## There are 4 steps to this function
## set a value
## get a value
## set a value for the inverse
## get a value for the inverse

makeCacheMatrix <- function(x = numeric()) {
     m <- NULL
     set <- function(y) {
         x <<- y
         m <<- NULL
     }
     get <- function() x
     setsolve <- function(solve) m <<- solve
     getsolve <- function() m
     list(set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
}

## This next function brings back the inverse of the matrix, checking to see if it 
## has already been calculated, if it has, it uses the cached version and lets you now that
## is the case. See the run below where it does just that.

cacheSolve <- function(x, ...) {
     m <- x$getsolve()
     if(!is.null(m)) {
         message("getting inverse data")
         return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setsolve(m)
     m
}

## BELOW IS A SAMPLE RUN WORKING FOR BOTH CASES

## myMatrix_object <- makeCacheMatrix(m1)
## cacheSolve(myMatrix_object)
##     [,1] [,2]
## [1,]    6    8
## [2,]    2    4
## cacheSolve(myMatrix_object)
## getting inverse data
##     [,1] [,2]
## [1,]    6    8
## [2,]    2    4
 
