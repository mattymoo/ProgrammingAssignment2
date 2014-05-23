##Overall, these functions work together to create a matrix, store it in cache, and take the inverse
##    of the matrix when called upon

#this function in particular will create a "matrix" object and store it in variable x

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                          #if no value is set, store as NULL, otherwise store in x and clear cache
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x                   #get the matrix stored in x        
        setmatrix <- function(solve) m <<- solve   #store the matrix in the cache
        getmatrix <- function() m                  #grab the matrix that is stored in the cache
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

#this function will compute the inverse of the matrix returned in "makeCacheMatrix.R"

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()            #return the value in the cache and store it in variable "m"
        if(!is.null(m)) {             #if cache has a value in it, return message "getting cached data" and return the matrix
                message("getting cached data")
                return(m)
        }
        data <- x$get()             #get the submitted matrix and store it as data
        m <- solve(data, ...)          #calculate the inverse of the matrix and store it in "m"
        x$setmatrix(m)
        m                             #display inverse matrix if called upon
}