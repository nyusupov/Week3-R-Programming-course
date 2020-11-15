## This function first creates a matrix and then can make an inverse function
## on it. It also don't always do it by default but first checks if it has
## already been created and if yes, it returns a message to.

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        set_inver <- function(inverse) inver <<- inverse
        get_inver <- function() inver
        list(set = set, get = get,
             set_inver = set_inver,
             get_inver = get_inver)
}
## Return a matrix that is the inverse of 'x'. If it already exists, it gives a
## message and prints it.

cacheSolve <- function(x, ...) {
        inver <- x$get_inver()
        if (!is.null(inver)){
                message("getting chached data")
                return(inver)
        }
        matr <- x$get()
        inver <- solve(matr, ...)
        x$set_inver(inver)
        inver
}