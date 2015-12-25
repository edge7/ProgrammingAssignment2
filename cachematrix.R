## makeCacheMatrix and cacheSolve are two functions that together allow users to
## create a special matrix object and to cache its inverse in order to speed up processing 
## in case inverse is already computed

## makeCacheMatrix gets as argument a Matrix (you can call it without arguments if needed).
## it creates a special matrix object which has following methods callable by user:
## set, get, setinverse and getinverse
## set -> set the Matrix (use this function if you call makeCacheMatrix without arguments 
##                        or if you want to change the matrix itself
## get -> get the Matrix 
## setinverse -> set the inverse of the matrix 
## getinverse -> get the inverse of the matrix

makeCacheMatrix <- function( x = matrix() ) {
    
    #Inverse is NULL at the beginning
    inverse <- NULL
    
    # Set matrix for which inverse should be computed 
    set <- function( matrix )
    {
      #Please, note that <<- operator allow user to assign objects in the parent environment
      x <<- matrix
      #When matrix is set, inverse must be set NULL
      inverse <<- NULL
    }
    
    # Get the Matrix  
    get <- function() x 
    
    # Set Inverse
    setinverse <- function( inv )
    {
      #Please note that <<- operator allow to set objects in the parent environment
      inverse <<- inv
    }
    
    # Get inverse back 
    getinverse<- function() inverse
    
    # @Return value: a list with the methods described above
    list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve gets as argument a 'MakeCacheMatrix' object and returns back its inverse,
## If possible it will skip the inverse computation 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    #If inverse is not null then skip the computation and just returns it back 
    if( !is.null( inverse) ) 
    {
      message("getting cached data")
      return(inverse)
    }
    #Getting matrix, I am assuming matrix is correctly set and invertible 
    data <- x$get()
    #Using solve to compute the inverse
    inverse <- solve(data, ...)
    #Set the inverse for next calling, i.e.: cache it
    x$setinverse(inverse)
    #Return back the inverse
    inverse
}
