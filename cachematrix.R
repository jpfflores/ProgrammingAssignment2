## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Sorry for my poor english, it is not my native language

## This function only stores data, there is no data manipulation
makeCacheMatrix <- function (x = matrix()){
	mtxinv <- NULL #Cached inverse matrix
	set <- function(y) {
                x <<- y # Original matrix
                mtxinv <<- NULL #Clears the cache
        }
	get <- function() x # Gets the original matrix
	
	setinv <- function(inv) {
					mtxinv <<- inv
	}  #set inverse matrix
	getinv <- function() mtxinv # Recovers the inverse matrix
	list(set = set, get = get,
		 setinv = setinv,
		 getinv = getinv)
}

## Write a short comment describing this function
## This function takes the structure and verifies if there is a cached version of the inverse
cacheSolve <- function(x) {
       ## Return a matrix that is the inverse of 'x'
		inv <- x$getinv() # First get inverse matrix
		data <- x$get() # Then get original matrix to compare 
		## It tests if inv exists and is identical to inverse of original matrix. 
		## At this point, there is no reason to take the cached because it is calculated again, 
		## but I can not think of other way to test if the matrix was modified
        if(!is.null(inv) && identical(inv, solve(data)) ){
			message("getting cached data")
			return(inv)
        }
		## Get the inverse
        minv <- solve(data)
		## Store the matrix
        x$setinv(minv)
		## Shows the result
        minv
}
