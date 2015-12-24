## makeCacheMatrix and cacheSolve are two functions that together allow users to
## create a special matrix object and to cache its inverse in order to speed up computation
## in case inverse is already computed

## makeCacheMatrix gets as argument a Matrix (you can call it without arguments if needed).
## it creates a special matrix object which has following methods callable by user:
## set, get, setinverse and getinverse
## set -> set the Matrix (use this function if you call makeCacheMatrix without arguments)
## get -> get the Matrix 
## setinverse -> set the inverse of the matrix 
## getinverse -> get the inverse of the matrix, if available

makeCacheMatrix <- function( x = matrix() ) {
    
    #Inverse is NULL at the beginning
    inverse <- NULL
    
    # Set matrix for which inverse should be computed 
    set <- function( matrix )
    {
      x <<- matrix
      #When matrix is set, inverse must be set NULL
      inverse <<- NULL
    }
    
    # Get the Matrix  
    get <- function() x 
    
    # Set Inverse
    setinverse <- function( inv )
    {
      #Please note that <<- operator allow to set object in the parent environment
      inverse <<- inv
    }
    
    # Get Inverse back 
    getinverse<- function() inverse
    
    # @Return value: a list with the method described above
    list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve gets as argument a 'MakeCacheMatrix' object and returns back its inverse,
## If possible it will skip the inverse computation 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if( !is.null(inverse) ) 
    {
      message("getting cached data")
      return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
