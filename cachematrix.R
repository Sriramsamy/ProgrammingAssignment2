# makeCacheMatrix -> will receive a matrix as input and returns a 
##list of functions set, get, setmatrix and get matrix
#set -> This function will store the matrix received as input in the parent
## environment and will blank out cache variable m
#get -> will retrieve the input matrix
#setmatrix->will store the inverse matrix in cache variable m
#getmatrix->will retrieve the matrix in cache variable m

## makeCacheMatrix will be run once to create a list of functions. 
#if a new matrix to be set call the function set() from the list of functions

makeCacheMatrix <- function(x = matrix()) 
{
  m<-NULL
  set <-function (y)
  {
    x<<-y
    m<<-NULL
  }
  get <- function() x
  setmatrix <- function(nmat=matrix()) m <<- nmat
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}



#The returned list from makeCacheMatrix function should be passed as 
#input to cachesolve

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
  
    mat<-x$getmatrix()
    
    if(!is.null(mat))
    {
      message("getting cached data")
      return(mat)
    }
    data<-x$get()
    print(data)
    invMat<-solve(data)
    output<-x$setmatrix(invMat)
    return(output)
}

