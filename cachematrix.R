#################################################
## Programming Assignment 2 - cachematrix.R
#################################################

## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss 
## here). 
## Your assignment is to write a pair of functions that cache the inverse of a 
## matrix.

# 1. makeCacheMatrix: 
# makeCacheMatrix: This function creates a special "matrix" 
# object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



# 2. cacheSolve: 
# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache.
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if (!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

####### TEST ################

# Step 1: Create a matrix
A <- matrix(c(2, 1, 1, 2), nrow = 2, ncol = 2)

# Step 2: Use makeCacheMatrix to wrap it
cachedMatrix <- makeCacheMatrix(A)

# Step 3: Call cacheSolve() to compute and cache the inverse
inverse1 <- cacheSolve(cachedMatrix)
print(inverse1)  # This will compute and print the inverse

# Step 4: Call cacheSolve() again to check if it uses the cache
inverse2 <- cacheSolve(cachedMatrix)
print(inverse2)  # Should print the same result and show "getting cached inverse"
