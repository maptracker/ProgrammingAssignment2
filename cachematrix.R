## A set of functions to calculate the inversion of a matrix and then
## store the result for fast recovery later.

## "Programming Assignment 2" from the Johns Hopkins "R Programming"
## Coursera course. The code below is mostly following the paradigm
## laid out by Dr. Peng's vector-mean-caching example:

## https://github.com/rdpeng/ProgrammingAssignment2

# I am putting function comments inside the function block.
# http://google-styleguide.googlecode.com/svn/trunk/Rguide.xml

# Usage:
#
#   source("cachematrix.R")
#   Make a 5 x 5 random matrix:
#   myMat <- makeCacheMatrix( randomMatrix( rows = 5 ) )
#   cacheSolve( myMat ) # calculate, cache, and retrive inversion
#   cacheSolve( myMat ) # retrieve cached inversion

# Alternate usage:
#
#   source("cachematrix.R")
#   Make a 1000 x 1000 random matrix:
#   myMat <- makeCacheMatrix( randomMatrix( rows = 1000 ) )
#   str( myMat$inversion() ) # calculate and cache inversion, then summarize
#   str( myMat$inversion() ) # Summarize cached inversion
#   myMat$checkInversion()   # Verify that our inversion is correct

makeCacheMatrix <- function( x = matrix() ) {
    # Defines an object holding a matrix and its cached inversion
    #
    # Arguments:
    #
    #   x: The matrix you wish to hold. If not provided, will be an
    #      empty matrix
    #
    # Returns:
    #
    # A list with the following components:
    #
    #        getMatrix(): Returns the provided matrix
    #        setMatrix(): Specifies a new matrix, clears the cache
    #     setInversion(): Stores the inversion of the matrix
    #     getInversion(): Recovers the A function to recover the inversion
    #
    # Bonus components:
    #
    #        inversion(): Object-internal calculate-and-cache method
    #   checkInversion(): Validates the matrix inversion

    
    # Locally-scoped variable to cache the inversion:
    invCache <- NULL
    
    setInversion <- function( inversionValue ) {
        # If <- were used here, then "invCache" would be locally
        # scoped to only *this* function (setInversion). <<- will
        # "find" the variable in the above containing environment and
        # set that instead
        invCache <<- inversionValue
    }
    
    # This function just blindly returns whatever was stored with setInversion
    getInversion <- function() invCache

    inversion <- function() {
        # I think this is a better caching paradigm to what is
        # described in the exercise. This object already has
        # everything it needs to internally perform an inversion, and
        # then cache the result
        action <- "recover"
        start <- Sys.time()
        if (is.null(invCache)) {
            # The cache has not been set. Create and return it
            action <- "create"
            print("Calculating inversion, please wait...")
            invCache <<- solve( x )
        }
        print(sprintf("Time to %s inversion: %.3f msec", action,
                      1000 * (Sys.time() - start)))
        invCache
    }

    checkInversion <- function() {
        # The inversion should be such that when a matrix is multiplied by it,
        # the result is the matrix itself. Double check
        orig  <- getMatrix() # The original (stored) matrix
        inv   <- inversion() # The inversion of it
        
        # An "pure" identity matrix of the appropriate size:
        identMat <- diag(nrow = nrow(orig), ncol = ncol(orig))
        # The "real" identity matrix when doing the matrix multiplication:
        print("Multiplying your matrix by the inversion...")
        identChk <- orig %*% inv

        # We can't use identical(), because there are some rounding
        # issues at the least significant figures, eg:
        
        #               [,1]          [,2]          [,3]          [,4]
        # [1,]  1.000000e+00 -3.330669e-16 -6.106227e-16 -5.551115e-17
        
        # That is, it's *almost* 1 and *almost* zero, but not quite.
        # Use all.equal(), which has a default "tolerance" of ~1.5e-8
        isOk <- all.equal(identMat, identChk)
        msg <- if (isOk) { "equals" } else { "does NOT equal" }
        
        print(sprintf("x %%*%% inversion %s identity matrix", msg))
        isOk
    }
    
    setMatrix <- function( newMatrix ) {
        # For some reason the exercise wants the matrix to be
        # redefinable. A better appraoch would be to just make a new
        # matrix object with makeCacheMatrix(), but we will implement
        # the feature as requested
        x <<- newMatrix
        # Need to clear any previously-set cache! Otherwise you will
        # return the inverse of the *old* matrix.
        invCache <<- NULL
    }

    getMatrix <- function() x  # Just return the matrix

    # Build and return the list. I am also setting "aliases" get() and
    # set() for the matrix recovery functions.
    rv = list(setMatrix = setMatrix, getMatrix = getMatrix,
              set       = setMatrix, get       = getMatrix,
              setInversion = setInversion, getInversion = getInversion,
              inversion = inversion, checkInversion = checkInversion )
    
    # Set a class for sanity checking in cacheSolve()
    oldClass(rv) <- "CachedMatrix"
    rv
}

cacheSolve <- function(x, ...) {
    # Calculate the inverse of a matrix, unless it has already been
    # calculated.
    #
    # Arguments:
    #
    #   x: An object created with makeCacheMatrix()
    #
    # Returns:
    #
    #   A matrix representing the inverse of the matrix held by x
    #
    # The returned value is the inverse cache held by x. If x does not
    # already have this value, it will create and store it. Note that
    # running x$inverse() is a more elegant way to do this.

    rv <- x$getInversion()
    if (is.null(rv)) {
        # The inversion has not yet been calculated
        # Recover the matrix from the object:
        mat <- x$getMatrix()
        # Solve it:
        print("Calculating inversion, please wait...")
        rv <- solve(mat)
        # Cache the value:
        x$setInversion(rv)
    } else {
        print("Using previously-cached inversion")
    }
    # Return the inversion, whether we got it from the cache or just
    # calculated it:
    rv
}

randomMatrix <- function( rows = 1000, cols = NULL, min = 1, max = 100) {
    # Generates a matrix of random integers for testing
    #
    # Arguments:
    #
    #   rows: The number of rows in the matrix, defaults to 1000
    #   cols: Number of columns, defaults to number of rows
    #    min: Minimum value of random integer, default 1
    #    max: Maximum value of random integer, default 100
    #
    # Returns:
    #
    # A rows x cols matrix of random numbers
    
    # If columns are not defined, then set them to number of rows:
    if (is.null(cols)) cols <- rows
    
    # Generate random integers to put into the matrix
    random <- runif( cols * rows, min = min, max = max + 1)
    
    # Create and return the matrix:
    matrix( as.integer(random), nrow = rows, ncol = cols)
}
