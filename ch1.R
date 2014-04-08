## Wed Apr 2, 2014
############# Problem 1.4 #############
#######################################

# Generating the data
size = 20

setClass(Class="questionSet",
         representation(
             X="matrix",
             m="numeric",
             b="numeric",
             f="numeric",
             classes="numeric"
          )
)

overunder <- function (vector, fLine) {
    #### THIS IS NOT CORRECT
    fhat <- vector %*% fLine + 1
    return (sign(fhat))
}

makeData <- function(size, dim) {
    # random [size] points
    X <- matrix(runif(dim*size, -1, 1), size, dim)

    ## random line
    fLine <- runif(dim, -1, 1)
    ## for 2-D plot, make sure we can see the line in the plot.
    ## i.e. make -1 =< m, b <= 1
    m <- fLine[1]
    b <- fLine[2]
    fLine[1:2] <- c(fLine[1]/fLine[2], -1/fLine[2])

    classes <- apply(X, 1, overunder, fLine=fLine)
    return(new("questionSet",
               X=X,
               m=m,
               b=b,
               f=fLine,
               classes=classes
               ))  
}

# Drawing the data and Perceptron Learning Algorithm
perceptron <- function(size, dim, isChart, partg=1, eta=0) {
    data <- makeData(size, dim)
    # add coordinate x0 = 1
    Xp <- cbind(rep(1,size), data@X)   
    if (eta==0) {
        tvect <- c()
        for (i in 1:partg) {
            # start at w0 = 0
            w <- rep(0,dim + 1)
            t = 0
            matrixSigns <- sign(w %*% t(Xp))
            while ( any( matrixSigns != data@classes) & t <size*10 ) {
                samp <- sample( which( matrixSigns != data@classes), 1)
                y <- data@classes[samp]
                ## update PLA
                w <- w + y * Xp[samp,]
                matrixSigns <- sign(w %*% t(Xp))
                t <- t+1
            }
            tvect[i] <- t
        }
    } else {
        ## Adaline; Adaptive Linear Nueron algorithm for Perceptron Learning.
        w <- rep(0,dim + 1)
        t = 0
        matrixVals <- w %*% t(Xp)
        matrixSigns <- sign(matrixVals)
        while ( any( matrixSigns != data@classes) & t <1000 ) {
            samp <- sample( which( matrixSigns != data@classes), 1)
            y <- data@classes[samp]
            ## is y(t)*rho(t) close enough?
            if (y * matrixVals[samp] <= 1) {
                ## update PLA
                w <- w + eta * (y - matrixVals[samp]) * Xp[samp,]
                matrixSigns <- sign(w %*% t(Xp))
            } ## otherwise, don't update the algorithm
            t <- t+1
        }
        
        ## Error calculations
        errorRate <- sum( which( matrixSigns != data@classes) ) / size
        tvect <- c(t, errorRate)
    }

    # make a chart?
    ## warning: this Chart will not make sense for dim > 2, because it
    ## just draws out the first two dimensions.
    if( isChart == TRUE) {
        # the plot
        plot.new()
        title <- paste("Linearly separable data set of size", size)
        plot(data@X, xlab="x", ylab="y",xlim=c(-1, 1), ylim=c(-1,1), main=title, col=(data@classes / 2 + 2.5))
        abline(a=data@b, b=data@m)
        legend(x=0.7, y=1, legend=c("group 1", "group 2"), pch=1, col=c(2,3))
        targetLineLabel <- paste("target fnc: y =",
                           toString(round(data@m, 2)),
                           "x",
                           "+",
                           toString(round(data@b, 3)))
        text(x=0.1, y=0.4, labels=targetLineLabel)

        g.int <- - w[1] / w[3]
        g.slope <- - w[2] / w[3] 
        gLineLabel <- paste("hypothesis fnc: y =",
                           toString(round(g.slope, 2)),
                           "x",
                           "+",
                           toString(round(g.int, 3)))
        text(x=0.1, y=0.2, labels=gLineLabel)
        abline(a=g.int, b=g.slope, lty=2)
    }
    if( partg==1) {
        if (eta==0) {
            answers <- rbind(c(tvect, rep(NA, dim)), c(1, data@f), w/w[1])
        } else {
            answers <- rbind(c(tvect, rep(NA, dim-1)), c(1, data@f), w/w[1])
        }
    } else {
        answers <- tvect
    }
    return (answers)
}

# part a) and b) c) 
perceptron(20, 2, TRUE)
# Lines both correctly separate the data, but still widely different
# since there are so few data points. Running a bunch of times, takes
# ~5-21 iterations to complete.

# part d)
perceptron(100, 2, TRUE)
# Lines much closer, understandably since there is more data. Takes
# ~18-71 iterations to complete.

# part e)
perceptron(1000, 2, TRUE)
# Lines so close that they are difficult to tell apart visually in the graph. Both slope and intercept are ~ < 0.01 in difference. (~ < 1 %)

## part f)
perceptron(1000, 10, FALSE)
## Ran a few times. Takes ~3000-9000 iterations.

## part g)
## x(t) is already always generated randomly, sampled from the
## list of misclassified points.
histData <- perceptron(1000, 10, FALSE, 100)
hist(histData)
## Mostly it takes ~6000-7000 iterations to converge.

## part h)
## Accuracy increases with number of data points N, decreases with
## number of dimensions d.  Running time increases with number of data
## points N, but increases multiplicatively with number of dimensions
## d.


############# Problem 1.5 #############
#######################################

## part a)
trainingSet <- makeData(100, 2)
testSet <- makeData(10000, 2)
etaPerceptron <- perceptron(100,2,TRUE, 1, eta=100)
## for eta=100, takes ~29-59 iterations

## part b)
perceptron(100,2,TRUE, 1, eta=1)
## takes ~70-90 iterations, where the
## error rate is 0.
## Rarely, maxes 1000 iterations, error rate 0.78.

## part c)
perceptron(100,2,TRUE, 1, eta=0.01)
## takes ~ 7-200 iterations, high variance in number of iterations.
## error rate is 0

## part d)
perceptron(100,2,TRUE, 1, eta=0.0001)
## usually 30-40 iterations, where error rate is 0
## Rarely, between 2-28 iterations, get -Inf. Possibly rounding error.

## part e)
## Generally high variation in number of iterations, though it seems that with eta approaching 0, the number of iterations might be on average smaller. However, with very small eta (10e-4), sometimes it did not converge to a solution with finite weights w.


