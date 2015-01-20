## makeCacheMatrix: it creates a special "matrix" object 
## that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
		
		# initilize to store the inverse
		invmatri <- NULL
		
		# set should be able to alter the matrix
		# it invalidates the cache
		set <- function(y) {
			x <<- y
			invmatri <<- NULL
		}
		
		# get just returns the raw matrix
		get <- function() {
			x
		}
		
		# setinvmatri sets the invmatri variable
		# should be used only by cacheSolve
		setinvmatri <- function(invmatri) {
			invmatri <<- invmatri
		}
		
		# getinvmatri gets the cached inverse
		getinvmatri <- function() {
			invmatri
		} 
		
		# return the special matrix
		list(set = set, 
			 get = get,
			 setinvmatri = setinvmatri,
			 getinvmatri = getinvmatri)
		
}


## cacheSolve: it computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix
## has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

		# get the cached inverse
		invmatri <- x$getinvmatri()
		
		# if the inverse is already cached, just return it
		if(!is.null(invmatri)) {
			message("getting cached inverse matrix")
			return(invmatri)
		}
		
		# otherwise, get the matrix 
		matrix <- x$get()
		
		# calculate the inverse
		invmatri <- solve(matrix, ...)
		
		# and cache it
		x$setinvmatri(invmatri)
		
		# return the result
		invmatri
}

# Test codes
# aMat <- makeCacheMatrix( matrix(1:4, nrow=2, ncol=2) )
# 
# cacheSolve(aMat)
# 
# aMat <- makeCacheMatrix( matrix(5:8, nrow=2, ncol=2) )
# 
# aMat <- makeCacheMatrix(diag(3))
# 
# cacheSolve(aMat)
# 
# cacheSolve(aMat)