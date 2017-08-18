#Assignment: Caching the Inverse of a Matrix

#Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse 
#of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that 
#we will not discuss here). 
#Below I describe a pair of functions that cache the inverse of a matrix.
#Please give me feedback to solve this and get my reward.


# first clear environment
rm(list=ls())


#Function 1: function description to make a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        var_inv <- NULL
        set <- function(y) {
                x <<- y
                var_inv <<- NULL
        }
        get <- function() x 
        setInverse <- function(solve) var_inv <<- solve
        getInverse <- function() var_inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


#Function 2: cacheSolve
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
#should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        var_inv <- x$getInverse()
        if (!is.null(var_inv)) {
                message("getting cached data, it's working! ;-)")
                return(var_inv)
        }
        data <- x$get()
        var_inv <- solve(data, ...)
        x$setInverse(var_inv)
        var_inv
}


##some testing

#create matrix
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))

#viewing data data
my_matrix$get()

#caching the data
cacheSolve(my_matrix)

#now again, but directly from my cached data
cacheSolve(my_matrix)


#-------------------
#creating a new matrix
my_matrix <- makeCacheMatrix(matrix(c(3,2,1,4), 2, 2))

#viewing data data
my_matrix$get()


cacheSolve(my_matrix)

#now again, but directly from my cached data
cacheSolve(my_matrix)

#viewing data data
my_matrix$get()

