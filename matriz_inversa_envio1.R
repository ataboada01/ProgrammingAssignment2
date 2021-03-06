 
 
# makeCacheMatrix creates a list containing a function to 
# 1. set the value of the matrix 
# 2. get the value of the matrix 
# 3. set the value of inverse of the matrix 
# 4. get the value of inverse of the matrix 



makeCacheMatrix <- function(x = matrix()) { 
     inv <- NULL 
     set <- function(y) { 					    #defines a function to set the matrix x to a new matrix y and resets the inverse to NULL
         x <<- y 
         inv <<- NULL 
     } 
     get <- function() x 					    # returns the matrix x	
     setinverse <- function(inverse) inv <<- inverse      # sets the inverse , inv to the inverse matrix 
     getinverse <- function() inv 				    # returns the inver
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) # returns the special matrix containing all functions
												  # naming the list elements allows us to access the functions by name using $
 } 
 
 
 
 
# The following function returns the inverse of the matrix. It first checks if 
# the inverse has already been computed. If so, it gets the result and skips the 
# computation. If not, it computes the inverse, sets the value in the cache via 
# setinverse function. 
 
 

 cacheSolve <- function(x, ...) { 
     inv <- x$getinverse() 
     if(!is.null(inv)) {					    # if the result is FALSE  cachemean() takes the matrix from the input object and calculates		 
         message("getting cached data.") 			    # the inverse ,uses the setinverse function to set the inverse matrix  	
         return(inv) 
     } 
     data <- x$get() 
     inv <- solve(data) 
     x$setinverse(inv) 
     inv 
 } 
 
