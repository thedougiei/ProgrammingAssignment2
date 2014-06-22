## This is a programming assignment for the following modUle
## https://class.coursera.org/rprog-004/human_grading/view/courses/972139/assessments/3/submissions

## there are 2 functions supplied
##
## makeCacheMatrix() and cacheSolve()
##
##
## To execute and test, the sequence of steps would be
##
##		 m<- makeCacheMatrix( )
## 		 m$set( .. your square matrix ...)
## 		 m$get()
##  	 cacheSolve( m )
##  
## 		 and again... should be cached now
##  	 cacheSolve( m )



## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL
	set <- function(y) {
                x <<- y
                m <<- NULL
        }
	get <- function() x
	
	setinverse <- function(solve) m <<- solve(x)
	getinverse <- function()  m
	list(set = set, get = get,
             setinverse= setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

## This function returns the inverse of a square matrix
## The inverse calculation is only executed if there is no previously cached value



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		m <- x$getinverse()
		if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}









