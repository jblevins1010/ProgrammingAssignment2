## The two functions below create an R object that stores the value
## of a matrix and caches the inverse of the matrix. These values
## are preserved in the object using the <<- operator.


## makeCacheMatrix takes an invertible matrix as it's argument and
## returns an R object that is a list of functions that let you:
## set() the matrix, get() the matrix, setinverse() of the matrix,
## and getinverse() of the matrix. The matrix 'x' and it's inverse
## 'xinverse' are preserved using the <<- operator.
makeCacheMatrix <- function(x = matrix()) {
        xinverse <- NULL
        set <- function(y) {
                x <<- y
                xinverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) xinverse <<- inverse
        getinverse <- function() xinverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve takes an R object that contains an invertable matrix
## created using the makeCacheMatrix function, and sets and returns
## the inverse of the matrix. If the inverted matrix has already 
## been calculated, then the cached version of the inverted matrix 
## is returned, saving the costly step of calculating the inverse
## more than once.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xinverse <- x$getinverse()
        if(!is.null(xinverse)) {
                message("getting cached data")
                return(xinverse)
        }
        xmatrix <- x$get()
        xinverse <- solve(xmatrix)
        x$setinverse(xinverse)
        xinverse
}
