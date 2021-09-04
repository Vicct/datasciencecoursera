makeCacheMatrix <- function(x = matrix()){
## makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse.
## Use this function in the following way, for example:  
## Newmatrix<-makeCacheMatrix(matrix(c(4, 2, 2,
##                                     2, 3, 1,
##                                     2, 1, 3), nrow=3, byrow=TRUE))  
## Other example:
## Newmatrix<-makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
##
  library(matlib)                          ##Loads the library
  if (det(x)!=0){                          ##(det) function dertermines if the matrix is inversable
    invx <- NULL
    
    set <- function() {                    ## Set the matrix function
      y <<- x
      
      invx <- NULL
      }
      
    get <- function(){x}                   ## Get the matrix function. For example; Newmatrix$get
    
    setinverse <- function(){              ## Function to apply the inverse. For example; Newmatrix$setinverse
      inv  = inv(x)
      invx <<- inv
      }
    
    getinverse <- function(){              ##Function to get the inverse after applyig the setinverse. For example; Newmatrix$getinverse
      if(!is.null(invx)){
        invx
      }else{
        invx <- NULL
        message("NULL")
      }}
    
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

  }else{                                  ## If the matrix is not inversable will appear the following message.
   message("The matrix has not an inverse") 
  }

}

cachesolve <- function(x, ...)
{
## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.
  invx<-x$getinverse()                             ## This line applies the getinverse function
  
  if(is.null(invx)){                               ## Checks if the function is NULL
    
    invx <- x$setinverse()                         ## If function's inverse it is NULL, the function will apply the setinverse
    
    invx

  }else{                                           ## If function's inverse is already calculated, it was cached.
    
  message("Getting from cache")
  return(invx)

  }
  
}