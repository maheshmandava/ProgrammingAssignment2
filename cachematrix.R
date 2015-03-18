## makeCacheMatrix will create an object that stores a matrix. 
## cachesolve will provide the inverse of the makeCacheMatrix. 

## this function creates a matrix with a list containing set_mat,get_mat,set_inv,get_inv

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set_mat<-function(y){
    x<<-y
    inv<<-NULL                   ##initially assigning the inverse value as NULL
  }
get_mat<-function()x
set_inv<-function(solve)inv<<-solve  ##Cacheing the value of the inverse 
get_inv<-function()inv
list(set_mat=set_mat,get_mat=get_mat,set_inv=set_inv,get_inv=get_inv)
}


## this function first checks the cache to see if the inverse is available and if not then it will calculate.

cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
  
  inv<-x$get_inv()
  if(!is.null(inv))                    ##Checking for the inverse value in cache 
  {
    message("getting cached data")           
    return(inv)
  }
  data<-x$get_mat()
  inv<-solve(data,...)               ##Calculating the inverse
  x$set_inv(inv)                     ##Caching the inverse
  inv
}
