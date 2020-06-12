## using makeCacheMatrix which creates a special "matrix" object that can cache its inverse

## Pair of function that can cache the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
         i<-NULL
     ## using the inverse property
     
     setmatrix<- function(matrix){
     ##setting the matrix
                m<<- matrix
                i<<- NULL
                 }
     getmatrix<- function(){
                  m
                  ##returning the matrix
                 }
     setinverse<- function(inverse){
                 i<<- inverse
                 ##setting the inverse of matrix
                 }
    getinverse<- function(){
                i
                ##getting the inverse of matrix
                 }
      
      list(setmatrix = setmatrix , 
           getmatix = getmatrix,
           setinverse = setinverse,
           getinverse = getinverse)
                 

}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed),
##then cacheSolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        i<- x$getinverse
         
         if(!is.null(i)){
         ##if already set then returning the inverse
         message("getting cached data")
         return(i)
         }
         ##getting data
         data<-x$getmatrix
         
         ##inverse using matrix multiplication
         i<- solve(data) %*% data
         
         x$setinverse(i)
        ## Return a matrix that is the inverse of 'x'
        i
}
