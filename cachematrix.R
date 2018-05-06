## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function creates a specials matrix object that will cache the inverse of the matix
# 1. define the matrix function
# 2. set the value (x) and clear the cache (m)
# 3. define the function that gets the values of the matrix
# 4. define the function that calculates the inverse
# 5. list out the functions


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL  
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function()
                x
        
        setinverse <- function(solve)
                m <<- solve
        
        getinverse <- function()
                m
        
        list(
                set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
}


## Write a short comment describing this function

# This function returns the the matrix inverse either by calculation or from cache
# 1. fetch the cached value and store in m
# 2. evaluate m
# 3. if m is not NULL, return m
# 4. if m is NULL, calculate the inverse, cahche it, and then return it

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        
        m <- solve(data, ...)
        
        x$setinverse(m)
        
        m
}

# Sample Data Test
# 1. instantiate a 3x3 matrix with data
# > x<-rbind(c(-2.3, 1.8, 7.8),c(1.3, 4.5, -2.3), c(.8,-1.2,6.3))
# > x
#      [,1] [,2] [,3]
# [1,] -2.3  1.8  7.8
# [2,]  1.3  4.5 -2.3
# [3,]  0.8 -1.2  6.3

# 2. instantiate the special cache matrix object 
# > m<-makeCacheMatrix(x)
# > m$get()
#      [,1] [,2] [,3]
# [1,] -2.3  1.8  7.8
# [2,]  1.3  4.5 -2.3
# [3,]  0.8 -1.2  6.3

# 3. get the inverse
# > cacheSolve(m)
#             [,1]       [,2]        [,3]
# [1,] -0.21842112 0.17668297  0.33492945
# [2,]  0.08561015 0.17693903 -0.04139673
# [3,]  0.04404271 0.01126674  0.10831434
# this is the calculated result because no message was given

# 4. get the inverse again (it should come from cache with a message)
# > cacheSolve(m)
# getting cached data
#             [,1]       [,2]        [,3]
# [1,] -0.21842112 0.17668297  0.33492945
# [2,]  0.08561015 0.17693903 -0.04139673
# [3,]  0.04404271 0.01126674  0.10831434
# this is from cache as indicated by the message
