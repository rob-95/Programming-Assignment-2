## My functions comprised two functions: "makeCacheMatrix", and "cacheSolve".
## You can cache an matrix using "makeCacheMatrix" function and
## You can get inversed matrix using "cacheSolve" function.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x<<-y
    inv<<-NULL
  }
  get <- function() {x}
  setinverse <- function(inverse){inv <<-inverse}
  getinverse <- function(){inv}
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    retun(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv 
}

#For example:
x <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
#"cacheSolve" function calculate the inverse of the matrix previously defined in x
cacheSolve(x)
