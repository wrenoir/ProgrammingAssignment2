# steps -->
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeMatrix <- function(x=matrix()){
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  seti <- function(inverse) i <<- inverse
  geti <- function() i
  list (set=set, get=get, setinverse=seti, getinverse=geti)
}

cacheinverse <- function (x,...){
  i <- x$geti()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$seti(i)
  i
}