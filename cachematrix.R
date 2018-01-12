## Once upon a time there was a little, old lady who thought "I am getting too old for my
## job.  My back hurts, my feet ache, and I would like a job where I can sit and play on 
## the computer all day."  This little, old lady decided to try coding in R and this
## is the result..............

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
		set <- function(y=matrix()){
				x <<- y
				m <<- NULL
		}
		get <-function() x
		setinverse <- function (inverse) m <<- inverse
		getinverse <- function () m
		list (set=set, get=get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by make Cache Matrix

cacheSolve <- function(x, ...) {
		m <- x$getinverse ()
		if (!is.null(m)){
			message("getting cached data")
			return(m)
		}
		data <- x$get()
		m <- solve(data,...)
		x$setinverse(m)
		m
}
