## This set of functions allow for calculations on a input to be cached so they
## can be re-called later without having to perform the calculation again.
## In this case, we will be using matrix inversion as an example.

## The first function produces a list of functions that will make the input 
## cacheable. It is setting up an environment where the defined functions act as
## methods within the parent function's environment or class. By using the <<-
## operator within the two set functions, they can "see" and use the variables
## defined in the parent environment rather than be closed off and thought of as
## completely different variables usable only within those functions.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinversion <- function(inversion) inv <<- inversion
      getinversion <- function() inv
      list(set = set, get = get,
           setinversion = setinversion,
           getinversion = getinversion)
}


## This function calls the functions defined and listedin the first function, if 
## the matrix inversion is already performed, it can access it from the 
## makeCacheMatrix()'s getinversion() method, if it is is still a NULL, it will 
## get the data with get() then actually perform the calculation with the
## solve () function.

cacheSolve <- function(x, ...) {
        inv <- x$getinversion()
        if(!is.null(inv)) {
              message("getting cached data")
              retrun(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinversion(inv)
        inv
}

## test the functions
test <- makeCacheMatrix(matrix(c(22, 1, 44, 7, 2, 5 , 3, 8, 3), 3, 3))
cacheSolve(test)
