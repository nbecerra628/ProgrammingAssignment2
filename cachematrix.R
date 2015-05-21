#
# This function includes for functions that compute the inverse of a squared matrix
# that is created by the makeChaceMatrix function. The set function changes the matrix stored
# in the main function. The get function returns the matrix created by the makeCache function.
# The setinv function store the inverse matrix and the getinv function returns the inverse matrix.
# Finally the list ( ) command stores the 4 functions inside the makeCacheMatrix function

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL                               
   set <- function(y) {                      
      x <<- y                                
      inv <<- NULL                         
   }
   get <- function() {                       
      x                                      
   }
   setinv <- function(solve) {               
      inv <<- solve                          
   }
   getinv <- function() {                    
      inv                                    
   }
   list(set = set, get = get,                
        setinv = setinv,
        getinv = getinv)

}


## The function computes the inverse matrix of the matrix provided by the function 
# makeCache Matrix.  If the inverse has already been calculated, it gets the inverse from chace
# and avoids the computation. In this last case returns the message "getting from cached data"

cacheSolve <- function(x, ...) {
 inv <- x$getinv()                        
   if(!is.null(inv)) {                      
      message("getting cached data")        
      return(inv)
   }
   data <- x$get()                          
   inv <- solve(data, ...)                  
   x$setinv(inv)                            
   inv       
}


