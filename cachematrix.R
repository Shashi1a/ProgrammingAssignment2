#### this function used lexical scopping to cache inverse of the matrix
#### this allows one to compute inverse and if we need it later for computation
#### and the matrix is same then the inverse can be obtained from the cache rather than
### computing it from the scratch.
### Computing inverse can take a lot of time and the time takes increase as one increaes
### the matrix size. Thus, it's a good idea to cache the inverse incase we want to use
### it later purposes.



makeCacheMatrix <- function(x = matrix()) {
        
        invm<-NULL # inverse of matrix set to null
        
        #setting the matrix elements and inverse 
        ## objectname$set(matrix) to initialize a new matrix and set invm to NULL 
        ## 
        set<-function(y){
          x<<-y # setting x in the parent environment
          invm<<-NULL # setting invm 
        }
        
        ## return the matrix(call this function to check the matrix) \
        ## objectname$get to check the matrix
        get<-function() x 
        
        ## set the setinv and getinv variable
        
        ## objectname$getinv will output the inverse value
        
        setinv <-function(solve) invm<<-solve
        getinv <-function() invm
        
        ## list containing the functions this is the object that is returned when
        ## this function is called
         list(set=set,
              get=get,
              setinv=setinv,
              getinv=getinv)
  }



cacheSolve <- function(x, ...) {
      ## Return a matrix(invm) that is the inverse of matrix 'x'
      invm <-x$getinv()
    
      ## check if the value of inverse is null(not computed) 
      ## if the inverse is present it's value is obtained from the cache
      
      if(!is.null(invm)){
        message("getting cached inverse")
        return(invm)
      }
      
      ## get the matrix from the object 'x'
      data<-x$get()

      ## compute the inverse 
      invm<-solve(data)
  
      ## set the inverse varaiable with the inverse value
      x$setinv(invm)
      invm
      
      }
