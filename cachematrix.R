## Programming Assignment 2
## 
## Create a function to store and recall values from the environment 
## and minimize a performing a redundant complex calculation. 

## Function that contains a list of 4 sub-functions that
##	store the value of a matrix
##	return the value of a matrix
##	store the output value or result of the inverse matrix calculation
##	return the output value or result of the inverse matrix calculation

makeCacheMatrix <- function(x = matrix()) {

    ## output of the inverse matrix calculation, initialize as NULL
    inv <- NULL
    
    ## store a matrix and set inv to NULL to ensure a new calculation is performed
    set <- function(y = matrix) { 
        x <<- y
        inv <<- NULL
    }
    
    ## return the matrix
    get <- function() { 
        x
    }
    
    ## store the result of performing the inverse matrix calculation
    setinv <- function(calc_inv) { 
        inv <<- calc_inv
    }
    
    ## return the result of a previously performing inverse matrix calculation
    getinv <- function() { 
        inv
    }
    
    ## deine the list of functions that can be called
    list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Function that performs inverse matrix calculation if not previously performed

cacheSolve <- function(x, ...) {

      ## lookup the current result for the inverse matrix calculation
      inv <- x$getinv()
  
      ## evaluate result and if not null, end the function by returning the result and a message 
      if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
  	    } 
  
	      ## if the result is null, lookup the matrix stored in cache 
	      data <- x$get()
  
              ## perform the inverse matrix calculation on the matrix
	      calc_inv <- solve(data)
      
              ## update the environment with the result of the calculation
      	      x$setinv(calc_inv)
  
              ## end funtion by displaying result of the calculation
              return(calc_inv)

}
