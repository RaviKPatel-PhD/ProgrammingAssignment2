##Caching the Inverse of a Matrix:
##Matrix caching the inverse of a matrix rather than compute it over and over.


##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) { #defining that X is a matrix
  inv <- NULL #set inv to NULL meaning it will be a variable used later in the funtion
  set <- function(y) {  #set takes the arguement y which is a matrix, not explictly defined yet
    x <<- y   #y in the parent enviornment is equal to x defined in the function
    inv <<- NULL #sets the vallue of inv to NULL in the parent enviornment. Forces cachesSolve to recalculate the matrix instead of using a cached value
  }
  get <- function() x  #get prints x (defined above, in the parent)
  setInverse <- function(inverse) inv <<- inverse #defines inv as the inverse
  getInverse <- function() inv #print inverse
  list(set = set,  #returns a list with 4 elements named set, get, setInverse, getInverse
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



##This function computes the inverse of the matrix created by makeCacheMatrix
##If the inverse has already been calculated (and the matrix has not changed), then it returns the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse() #defines that it is trying to getInvere from x
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  } #if inv is not NULL returns inv
  mat <- x$get() #define mat as matrix get
  inv <- solve(mat, ...) #inv is the solved mat
  x$setInverse(inv)
  inv #print inv
}


##testing the function
#>my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
#>my_matrix$get()
#[,1] [,2]
#[1,]    1    3
#[2,]    2    4
#>my_matrix$getInverse()
#NULL
#>cacheSolve(my_matrix)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#>cacheSolve(my_matrix)
#getting cached data
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#>my_matrix$getInverse()
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#>my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
#>my_matrix$get()
#[,1] [,2]
#[1,]    2    1
#[2,]    2    4
#>my_matrix$getInverse()
#NULL
#>cacheSolve(my_matrix)
#[,1]       [,2]
#[1,]  0.6666667 -0.1666667
#[2,] -0.3333333  0.3333333
#>cacheSolve(my_matrix)
#getting cached data
#[,1]       [,2]
#[1,]  0.6666667 -0.1666667
#[2,] -0.3333333  0.3333333
#>my_matrix$getInverse()
#[,1]       [,2]
#[1,]  0.6666667 -0.1666667
#[2,] -0.3333333  0.3333333
