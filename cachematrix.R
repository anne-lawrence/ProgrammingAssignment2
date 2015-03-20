## This pair of functions cache the inverse of a matrix (Note that the matrix must be invertible)

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 			## creates a special "matrix" object that can cache its inverse.
  
  inv <- NULL           # Initializes the 'inv' variable to NULL 
  
  set <- function(y) {  #'y' is the arg passed into 'makeCacheMatrix'
    
    x <<- y             # Sets 'x' for the function environment to 'y'
    
    inv <<- NULL        # Sets 'inv' for the 'makeCacheMatrix' environment to NULL
  }
  
  get <- function() x   
                        # Creates a function 'get' in the 'makeCacheMatrix'
                        # parent and assigns a matrix to it. Note that this only 
                        # makes sense within the context of the cacheSolve function.
  
  setinverse <- function(inverse) inv <<- inverse
                        # Takes a value ('inverse') and sets it to the
                        # value of 'inv' in the 'makeCacheMatrix' frame.
  
  getinverse <- function() inv
                        # Returns the value of 'm' from the makeCacheMatrix frame.
                        # Makes sense only in context of the 'cacheSolve' function.
  
  list(set = set, get = get,
       setinverse = setinverse, 
       getinverse = getinverse)
                        #Lists the values of the functions in the makeCacheMatrix frame.

}


## cacheSolve returns a matrix that is the inverse of 'x', the special matrix created by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        		  ## Returns a matrix that is the inverse of 'x'
 inv <- x$getinverse()   
                          # Goes to the'x' environment and assigns the 
                          # 'inv' value from that environment to this one.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)           
                          # If the 'x' environment has been evaluated before,
                          # the function prints the message and the value of inv.    
  }
  data <- x$get()         
                          # If this particular 'x' has never been
                          # evaluted before, pulls the x-vector into a
                          # local variable called 'data'.
  inv <- solve(data, ...)   
                          # Calculates the inverse of the matrix x by calling
                          # 'solve' function on the data local variable.
  
  x$setinverse(inv)       
                          # Assigns the calculated inv to the 'x' environment 
  
  return(inv)             
                          # Displays the calculated inverse.
}