## This function first creates a matrix to chace.

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
## Return the inverse of the matrix. If it already exists, it gives a
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
