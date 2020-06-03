## These set of functions cache the inverse of a Matrix. Once the inverse of a
# given matrix has been computed, the result is cached and the costly process 
# of computing its inverse does not need to be repeated.


## The first function, makeCacheMatrix, creates a list containing functions to:
#1. set the original matrix (x in the function)
#2. get the original matrix (x in the function)
#3. set the matrix's inverse (MI in the function)
#4. get the matrix's inverse (MI in the function)

makeCacheMatrix <- function(x = matrix()) {
    MI <- NULL
    set <- function(y) {
        x <<- y
        MI <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) MI <<- solve
    getinverse <- function() MI
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The second function, cacheSolve, first checks to see if the inverse of x has 
# already been computed. 
# If so, it gets the matrix's inverse from the cache and skips the computation. 
# Otherwise, it computes the inverse of x and sets the matrix's inverse in the 
# cache via the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    MI <- x$getinverse()
    if(!is.null(MI)) {
        message("getting cached data")
        return(MI)
    }
    data <- x$get()
    MI <- solve(data, ...)
    x$setinverse(MI)
    MI
}


## to test these functions, lets create a square matrix M
M=matrix(sample(1:10, 16, replace=TRUE), ncol=4)
# first, lets store the list from the first function into an object (cm)
cm=makeCacheMatrix(M)
# we then use cm as an argument for the cachSolve function
cacheSolve(cm)

# since this is the first time we compute the inverse of M, it was computed
# through cacheSolve and cached..
# If we run it again, the now cached matrix inverse is not computed, and
# message is returned : "getting cached data"
cacheSolve(cm)

# to double check if the result in-itself is correct:
M1=cacheSolve(makeCacheMatrix(M))#inverse computed through the function
M2=solve(M)# inverse directly computed
M1==M2# should be all TRUE

# another way to check: 
# M %*% cacheSolve(makeCacheMatrix(M)) should equal the identity matrix
I=M %*% cacheSolve(makeCacheMatrix(M))
round(I,1)
