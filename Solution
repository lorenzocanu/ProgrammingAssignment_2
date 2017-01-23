
 ## This function creates a special "matrix" object
 ## that can cache its inverse.
  
  makeCacheMatrix <- function(x = matrix()) {
 
     r <- NULL
     set <- function(y){
         x <<- y
         r <<- NULL
     }
     get <- function () x
     setInverse <- function(inverse) r <<- inverse
     getInverse <- function() r
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  }
  
  

 # "matrix" returned by `makeCacheMatrix` above. If the inverse has
 # already been calculated (and the matrix has not changed), then
 # `cacheSolve` should retrieve the inverse from the cache.
  
  cacheSolve <- function(x, ...) {
         ## Return a matrix that is the inverse of 'x'
     r <- x$getInverse()
     if(!is.null(r)){
         message("getting cached data")
         return(r)
     }
     
     data <- x$get()
 
     r <- solve(data, ...)
     x$setInverse(r)
     r
     
    
