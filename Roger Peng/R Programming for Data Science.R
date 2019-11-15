# Getting started with the R inteface
setwd("~/Studies/R/Roger Peng")
myfunction <- function() {
  x <- rnorm(100)
  mean(x)
}
ls()
myfunction()
dir()
source("R Programming for Data Science.R")

second <- function(x) {
  x + rnorm(length(x))
}

source("R Programming for Data Science.R")
ls()

## Entering Input
x <- 1
print(x)
x
msg <- "hello"
x<- # Incomplete expression

## Evaluation
x <- 5 # nothing printed
x      # auto printing occurs
print(x) # explicit printing  
x <- 10:30
x

## R Objects
##    character
##    numeric (real numbers)
##    integer
##    complex
##    logical (True/False)

## Attributes
##    names, dimnames
##    dimensions (e.g. matrices, arrays)
##    class (e.g integer, numeric)
##    length
##    other user-defined attributes/metadata

## Creating Vectors
x <- c(0.5, 0.6)        # numeric
x <- c(TRUE, FALSE)     # logical
x <- c(T, F)            # logical
x <- c("a", "b", "c")   # character
x <- 9:29               # integer
x <- c(1 + 0i, 2 + 4i)  # complex
x <- vector("numeric", length = 10)
x

## Mixing Objects
y <- c(1.7, "a")        # character
y <- c(TRUE, 2)         # numeric
y <- c("a", TRUE)       # character

# Explicit Coercion
x <- 0:6
class(x)
as.numeric(x)
as.logical(x)
as.character(x)
x <- c("a", "b", "c")
as.numeric(x)
as.logical(x)
as.complex(x)

## Matrices
m <- matrix(nrow = 2, ncol = 3)
m
dim(m)
attributes(m)
m<- matrix(1:6, nrow = 2, ncol = 3)
m
m <- 1:10
m
dim(m)
dim(m) <- c(2, 5)
m
x <- 1:3
y <- 10:12
cbind(x, y)
rbind(x, y)

## Lists
x <- list(1, "a", TRUE, 1 + 4i)
x
x <- vector("list", length = 5)
x

## Factors
x <- factor(c("yes", "yes", "no", "yes", "no"))
x
table(x)
unclass(x)
x <- factor(c("yes", "yes", "no", "yes", "no"), 
            levels = c("yes", "no"))
x

## Missing Values
##    is.na() test if object are NA
##    is.nan() test for NaN
x <- c(1, 2, NA, 10, 3)
is.na(x)
is.nan(x)

x <- c(1, 2, NaN, NA, 4)
is.na(x)
is.nan(x)  

## Data Frames
x <- data.frame(foo = 1:4, bar = c(T,T, F, F))
x  
nrow(x)
ncol(x)

## Names
x <- 1:3
names(x)
names(x) <- c("New York", "Seatle", "Los Angeles")
x
names(x)
x <- list("Los Angeles" = 1, Boston = 2, London = 3)
x
names(x)
m <- matrix(1:4, nrow = 2, ncol = 2)
dimnames(m) <- list(c("a", "b"), c("c", "d"))
m
colnames(m) <- c("h", "f")
rownames(m) <- c("x", "z")
m

## Data Types
##    atomic classes: numeric, logical, character, integer, complex
##    vectors, lists
##    factors
##    missing values
##    data frames and matrices

# Reading and Writing Data in R
data <- read.table("foo.txt")

# Reading in Larger Datasets with read.table
initial <- read.table("datatable.txt", nrows = 100)
classes <- sapply(initial, class) # gets the column classes from the 1st 100 rows for use in colClasses arg
tbAll <- read.table("datatable.txt",
                    colClasses = classes)

## Calculating Memory Requirements
options()$scipen
### [1] 0
options("scipen" = 16)
print(x)
### [1] 14400000000
## or 
format(x, scientific = F)
### [1] "14400000000"
##  1,500,000 rows x 120 cols x 8bytes/numeric = 14400000000 bytes = 14400000000/2^20 bytes/MB = 1,373.29 MB = 1.34GB

# Using Textual and Binary Formats for Storing Data
## dput-ting R Objects
y <- data.frame(a = 1, b = "a")
setwd("~/Source Files/R")
dput(y)
dput(y, file = 'y.R')
new.y <- dget("y.R")
new.y

## dump-ing R Objects
x <- "foo"
y <- data.frame(a = 1, b = "a")
dump(c("x", "y"), file = "data.R")
rm(x, y)
source("data.R")
y
x
str(y)

## Binary Formats
a <- data.frame(x = rnorm(100), y = runif(100))
b <- c(3, 4.4, 1 / 3)
setwd("~/Source Files/rda")
save(a, b, file = 'mydata.rda')
rm(list = ls())
load('mydata.rda')

##  Save everything
setwd("~/Source Files/RData")
save.image(file = 'mydata.RData')
rm(list = ls())
load('mydata.RData')

x <- list(1, 2, 3)
serialize(x, NULL)
setwd("~/Source Files/bin")
serialize(x, NULL)
con <- serialize(x, NULL)
save(con, file ='mydata.bin')
rm(list = ls())
load('mydata.bin')

# Interfaces to the Outside World
## File Connections
str(file)
con <- file("foo.txt", "r")
data <- read.csv(con)
close(con)

## Reading Lines of a Text File
setwd("~/Source Files/gz")
con <- gzfile("words.gz")
writeLines(c(1080, "10-point", "10th", "11-point", "12-point", "16-point", "18-point", "1st", 2, "20-point"), con = con)
x <- readLines(con, 10)
x

## Reading From a URL Connection
con <- url("http://localhost:4000/Locations Beginning with Ratho.html", "r")
x <- readLines(con)
head(x)

# Subsetting R Objects
## Subsetting a Vector
x <- c("a", "b", "c", "c", "d", "a")
x[1]
x[2]
x[1:4]
x[c(1, 3, 4)]
u <- x > "a"
u
x[u]
x[x > "a"]

## Subsetting a Matrix
x <- matrix(1:6, 2, 3)
x
x[1, 2]
x[2, 1]
x[1,]
x[,2]

## Dropping matrix dimensions
x <- matrix(1:6, 2, 3)
x[1, 2]
x[1, 2, drop = FALSE]
x <- matrix(1:6, 2, 3)
x[1,]
x[1,, drop = FALSE]


# Subsetting Lists
x <- list(foo = 1:4, bar = 0.6)
x
x[[1]]
x[["bar"]]
x$bar
x <- list(foo = 1:4, bar = 0.6, baz = "hello")
name <- "foo"
x[[name]]
x$name
x$foo

## Subsetting Nested Elements of a List
x <- list(a = list(10, 12, 14), b = c(3.14, 2.81))
x[[c(1, 3)]]
x[[1]][[3]]
x[[c(2, 1)]]


## Extracting Multiple Elements of a List
x <- list(foo = 1:4, bar = 0.6, baz = "hello")
x[c(1, 3)]

# Partial Matching
x <- list(aardvark = 1:5)
x$a
x[["a"]]
x[["a", exact = FALSE]]

# Removing NA Values
x <- c(1, 2, NA, 4, NA, 5)
bad <- is.na(x)
print(bad)
x[!bad]
x <- c(1, 2, NA, 4, NA, 5)
y <- c("a", "b", NA, "d", NA, "f")
good <- complete.cases(x, y)
good
x[good]
y[good]
head(datasets::airquality)
good <- complete.cases(datasets::airquality)
head(datasets::airquality[good,])


# Vectorised Operations
x <- 1:4
y <- 6:9
z <- x + y
z
z <- numeric(length(x))
for (i in seq_along(x)) {
  z <- x[i] + y[i]
}
z
x
x > 2
x >= 2
x < 3
y == 8
x - y
x * y
x / y

## Vectorised Matrix Operations
x <- matrix(1:4, 2, 2)
y <- matrix(rep(10, 4), 2, 2)
x * y
x / y
x %*% y


# Dates in R
x <- as.Date("1970-01-01")
x
unclass(x)
unclass(as.Date("1970-01-02"))

# Times in R
x <- Sys.time()
x
class(x)
p <- as.POSIXlt(x)
names(unclass(p))
p$wday

x <- Sys.time()
x
unclass(x)
x$sec
p <- as.POSIXlt(x)
p$sec

datestring <- c("January 10, 2012 10:40", "December 9, 2011 9:10")
x <- strptime(datestring, "%B %d, %Y %H:%M")
x
class(x)

# Operations on Dates and Times
x <- as.Date("2012-01-01")
y <- strptime("9 Jan 2011 11:34:21", "%d %b %Y %H:%M:%S")
x - y
x <- as.POSIXlt(x)
x - y
x <- as.Date("2012-03-01")
y <- as.Date("2012-02-28")
x - y
x <- as.POSIXct("2012-10-25 01:00:00")
y <- as.POSIXct("2012-10-25 06:00:00", tz = "GMT")
y - x

# Managing Data Frames with dplyr
## Select()
library(dplyr)
setwd('~/Source Files/rds')
chicago <- readRDS("chicago.rds")
dim(chicago)
str(chicago)
names(chicago)[1:3]
subset <- select(chicago, city:dptp)
head(subset)
head(select(chicago, -(city:dptp)))
i <- match("city", names(chicago))
j <- match("dptp", names(chicago))
head(chicago[, -(i:j)])
subset <- select(chicago, ends_with("2"))
str(subset)
subset <- select(chicago, starts_with("d"))
str(subset)

## filter()
chic.f <- filter(chicago, pm25tmean2 > 30)
str(chic.f)
summary(chic.f$pm25tmean2)
chic.f <- filter(chicago, pm25tmean2 > 30 & tmpd > 80)
select(chic.f, date, tmpd, pm25tmean2)

## arrange()
chicago <- arrange(chicago, date)
head(select(chicago, date, pm25tmean2), 3)
tail(select(chicago, date, pm25tmean2), 3)
chicago <- arrange(chicago, desc(date))
head(select(chicago, date, pm25tmean2), 3)
tail(select(chicago, date, pm25tmean2), 3)

## rename
head(chicago[, 1:5], 3)
chicago <- rename(chicago, dewpoint = dptp, pm25 = pm25tmean2)
head(chicago[, 1:5], 3)

## mutate()
chicago <- mutate(chicago, pm25detrend = pm25 - mean(pm25, na.rm = TRUE))
head(chicago)

## transmute
head(transmute(chicago, 
               pm10deterend = pm10tmean2 - mean(pm10tmean2, na.rm = TRUE), 
               o3detrend = o3tmean2 - mean(o3tmean2, na.rm = TRUE)
               ))

## group_by()
chicago <- mutate(chicago, year = as.POSIXlt(date)$year + 1900)
years <- group_by(chicago, year)
summarise(years, pm25 = mean(pm25, na.rm = TRUE), 
          o3 = max(o3tmean2, na.rm = TRUE), 
          no2 = median(no2tmean2, na.rm = TRUE)
          )
qq <- quantile(chicago$pm25, seq(0, 1, 0.2), na.rm = TRUE)
chicago <- mutate(chicago, pm25.quint = cut(pm25, qq))
quint <- group_by(chicago, pm25.quint)
summarise(quint, o3 = mean(o3tmean2, na.rm = TRUE), 
          no2 = mean(no2tmean2, na.rm = TRUE)
          )

## %>%
mutate(chicago, pm25.quint = cut(pm25, qq)) %>% 
  group_by(pm25.quint) %>% 
  summarise(o3 = mean(o3tmean2, na.rm = TRUE), 
            n02 = mean(no2tmean2, na.rm = TRUE)
            )
mutate(chicago, month = as.POSIXlt(date)$mon + 1) %>% 
  group_by(month) %>% 
  summarise(pm25 = mean(pm25, na.rm = TRUE), 
            o3 = max(o3tmean2, na.rm = TRUE), 
            no2 = median(no2tmean2, na.rm = TRUE)
            )


# if/else
x <- runif(1, 0, 10)
if (x >3) {
  y <- 10
} else {
  y <- 0
}
y
y <- if (x > 3) {
  10
} else {
  0
}
y

# for Loops
for (i in 1:10) {
  print(i)
}
x <- c("a", "b", "c", "d")
for (i in 1:4) {
  print(x[i])
}
for (i in seq_along(x)) {
  print(x[i])
}
for (letter in x) {
  print(letter)
}
for (i in 1:4) print(x[i])

## Nested for loops
x <- matrix(1:6, 2, 3)
for (i in seq_len(nrow(x))) {
  for (j in seq_len(ncol(x))) {
    print(x[i, j])
  }
}

# while Loops
count <- 0
while (count < 10) {
  print(count)
  count <- count + 1
}
z <- 5
set.seed(1)
while (z >= 3 && z <= 10) {
  coin <- rbinom(1, 1, 0.5)
  if (coin == 1) {
    z <- z + 1
  } else {
    z <- z - 1
  }
}
print(z)


# Repeat Loops
x0 <- 1
tol <- 1e-8
repeat {
  x1 <- computeEstimate()
  if (abs(x1 - x0) < tol) {
    break
  } else {
    x0 <- x1
  }
}

# next, break
for (i in 1:100) {
  if (i <= 20) {
    next
  }
}
for (i in 1:100) {
  print(i)
  if (i > 20) {
    break
  }
}

## Functions
f <- function() {}
class(f)  
f()
f <- function() {
  cat("Hello, world!\n")
}
f()
f <- function(num) {
  for (i in seq_len(num)) {
    cat("Hello, World!\n")
  }
}
f(3)
f <- function(num) {
  hello <- "Hello, World!\n"
  for (i in seq_len(num)) {
    cat(hello)
  }
  chars <- nchar(hello) * num
  chars
}
meaningoflife <- f(3)
print(meaningoflife)
f()
f <- function(num = 1) {
  hello <- "Hello, World!\n"
  for (i in seq_len(num)) {
    cat(hello)
  }
  chars <- nchar(hello) * num
  chars
}
f()
f(2)
f(num = 2)

## Argument Matching
str(rnorm)
mydata <- rnorm(100, 2, 1)
sd(mydata)
sd(x = mydata)
sd(x = mydata, na.rm = FALSE)
sd(na.rm = FALSE, x = mydata)
sd(na.rm = FALSE, mydata)
args(lm)
lm(data = mydata, y ~ x, model = FALSE, 1:100)
lm(y ~ x, mydata, 1:100, model = FALSE)
f <- function(a, b = 1, c = 2, d = NULL) {}

## Lazy Evaluation
f <- function(a, b) {
  a ^ 2
}
f(2)
f <- function(a, b) {
  print(a)
  print(b)
}
f(45)

## The ... Argument
myplot <- function(x, y, type = 'l', ...) {
  plot(x, y, type = type, ...)
}
mean
args(paste)
args(cat)

## Arguments coming after the ... Argument
args(paste)
paste("a", "b", sep = ":")
paste("a", "b", se = ":")

# Scoping Rules of R
# Binding Values to a Symbol
lm <- function(x) {x * x}
lm
search()

## Scoping Rules
f <- function(x, y) {
  x ^ 2 + y / z
}

# Lexical Scoping
make.power <- function(n) {
  pow <- function(x) {
    x ^ n
  }
  pow
}
cube <- make.power(3)
square <- make.power(2)
cube(3)
square(3)
cube
ls(environment(cube))
get("n", environment(square))
get("n", environment(cube))

## Lexical vs. Dynamic Scoping
y <- 10
f <- function(x) {
  y <- 2
  y ^ 2 + g(x)
}
g <- function(x) {
  x * y
}
f(3)
g <- function(x) {
  a <- 3
  x + a + y
}
g(2)
y <- 3
g(2)

# Application optimisation
## Constructor function
make.NegLogLik <- function(data, fixed = c(FALSE, FALSE)) {
  params <- fixed
  function(p) {
    params[!fixed] <- p
    mu <- params[1]
    sigma <- params[2]
    
    ## Calculate the Normal density
    a <- -0.5 * length(data) * log(2 * pi * sigma ^ 2)
    b <- -0.5 * sum((data - mu) ^ 2) / (sigma ^ 2)
    -(a + b)
  }
}
set.seed(1)
normals <- rnorm(100, 1, 2)
nLL <- make.NegLogLik(normals)
nLL
ls(environment(nLL))

## Try to minimize nLL function using Optim
optim(c(mu = 0, sigma = 1), nLL)$par

## Estimate one parameter whilst other is fixed using optimise as the problem is now one dimesional
nLL <- make.NegLogLik(normals, c(FALSE, 2))
optimise(nLL, c(-1, 3))$minimum
nLL <- make.NegLogLik(normals, c(1, FALSE))
optimise(nLL, c(1e-6, 10))$minimum

## Plotting the liklihood
## Fix mu to be equal to 1
nLL <- make.NegLogLik(normals, c(1, FALSE))
x <- seq(1.7, 1.9, len = 100)

## Evaluate nLL() at every point in x
y <- sapply(x, nLL)
plot(x, exp(-(y - min(y))), type = 'l', main = 'plot of chunk nLLfixMu')

## fix sigma to be equal to 2
nLL <- make.NegLogLik(normals, c(FALSE, 2))
x <- seq(0.5, 1.5, len = 100)

## Evaluate nLL() at every point in x
y <- sapply(x, nLL)
plot(x, exp(-(y - min(y))), type = 'l', main = 'plot of chunk nLLFixSigma')

## Loop Functions
## lapply() : Loop over a list and evaluate a function on each element
## sapply() : Same as lapply but try to simplify the result
## apply()  : Apply a function over the margins of an array
## tapply() : Apply a function over subsets of a vector
## mapply() : Multivariate version of lapply

## split()  : is also useful in conjunction with lapply()

# lapply()
## loops over a list iterating over each element in that list
## applies a function to each element of the list (a function that you specify)
## and returns a list (the l is for list)
lapply
?rnorm
x <- list(a = 1:5, b = rnorm(10))
lapply(x, mean)
x <- list(a = 1:4, b = rnorm(10), c = rnorm(20, 1), d = rnorm(100, 5))
lapply(x, mean)

## Use lapply() to evaluate a function multiple times each with a different argument
x <- 1:4
?runif
lapply(x, runif)

## the min and the max arguments are assed down to runif() every tim it gets called
x <- 1:4
lapply(x, runif, min = 0, max = 10)

## Creating a list that has two matrices
x <- list(a = matrix(1:4, 2, 2), b = matrix(1:6, 3, 2))
x

## Extract the first column of each matrix
## function here is anonymous
lapply(x, function(elt) {elt[, 1]})

## or

f <- function(elt) {
  elt[, 1]
}
lapply(x, f)

## sapply()
## If the result is a list where every element is length 1, then a vector is returned
## If the result is a list where every element is a vector of same length (> 1), a matrix is returned
## If it can't figure things out, a list is returned
x <- list(a = 1:4, b = rnorm(10), c = rnorm(20, 1), d = rnorm(100, 5))
lapply(x, mean)
sapply(x, mean)

# split()
## Takes a vector or other obects abd splits it into groups determined by a factor or list of factors
## x    is a vector (or list) or data frame
## f    is a factor (or coerced to one) or a list of factors
## drop indicates whether empty factors levels should be dropped
str(split)
x <- c(rnorm(10), runif(10), rnorm(10, 1))
f <- gl(3, 10)
?gl
split(x, f)
lapply(split(x, f), mean)

## Splitting a Data Frame
library(datasets)
head(airquality)

## split by month
s <- split(airquality, airquality$Month)
str(s)

## take column means
lapply(s, function(x) {
  colMeans(x[, c("Ozone", "Solar.R", "Wind")])
})
sapply(s, function(x) {
  colMeans(x[, c("Ozone", "Solar.R", "Wind")])
})

## Removing NAs with colMeans
lapply(s, function(x) {
  colMeans(x[, c("Ozone", "Solar.R", "Wind")], na.rm = TRUE)
})
sapply(s, function(x) {
  colMeans(x[, c("Ozone", "Solar.R", "Wind")], na.rm = TRUE)
})

## Splitting an R object according to levels defined in more than one variable
x <- rnorm(10)
f1 <- gl(2, 5)
f2 <- gl(5, 2)
f1
f2
interaction(f1, f2)

## interction may result in may levels that are empty
str(split(x, list(f1, f2)))
str(split(x, list(f1, f2), drop = TRUE))

# tapply()
## Can be thought of as a combination of split() and sapply()
## x        is a vector
## INDEX    is a factor or a list of factors (or else they are coerced to factors)
## FUN      is a function to be applied
## ...      conatins other arguments to be passed to FUN
## simplify indicates if the result be simplified
str(tapply)

## Simulate some data
x <- c(rnorm(10), runif(10), rnorm(10, 1))

## Define some groups with a factor variable
f <- gl(3, 10)
f
tapply(x, f, mean)
tapply(x, f, mean, simplify = FALSE)
tapply(x, f, range)
?range

# apply()
## x      is an array
## MARGIN is an integer vector indicating which margins should be retained
## FUN    is a function to be applied
## ...    is for other arguments to be passed to FUN
x <- matrix(rnorm(200), 20, 10)

## Take the mean of each column
apply(x, 2, mean)

## Take the mean of each row
apply(x, 1, sum)


## Col/Row Sums and Means
## rowSums = apply(x, 1, sum)
rowSums
## rowMeans = apply(x, 1, mean)
rowMeans
## colSums = apply(x, 2, sum)
colSums
## colMeans = apply(x, 2, mean)
colMeans
colMeans(x)

x <- matrix(rnorm(200), 20, 10)

## Get quantiles
apply(x, 1, quantile, probs = c(0.25, 0.75))

a <- array(rnorm(2 * 2 * 10), c(2, 2, 10))
apply(a, c(1, 2), mean)

## Faster with large arrays
rowMeans(a, dims = 2)

# mapply()
## FUN      is a function to apply 
## ...      contains R objects to apply over
## MoreArgs is a list of other arguments to FUN
## SIMPLIFY indicates whether the result should be simplified
list(rep(1, 4), rep(2, 3), rep(3, 2), rep(4, 1))
mapply(rep, 1:4, 4:1)

## Simulate random Normal variables
noise <- function(n , mean, sd) {
  rnorm(n, mean, sd)
}

## simulate 5 random numbers
noise(5, 1, 2)

## This only simulates 1 set of numbers, not 5
noise(1:5, 1:5, 2)

## mapply passes the sequence 1:5 separately to noise() to get 5 sets of random nyumbers with a different 
## length and mean
mapply(noise, 1:5, 1:5, 2)

## The same as 
list(
  noise(1, 1, 2), 
  noise(2, 2, 2),
  noise(3, 3, 2),
  noise(4, 4, 2),
  noise(5, 5, 2)
)


## Vectorising a Function
## mapply can be used to automatically vectorise a function
sumsq <- function(mu, sigma, x) {
  sum(((x - mu) / sigma) ^ 2)
}

## Generate some data
x <- rnorm(100)

## This is not what we want
sumsq(1:10, 1:10, x)

## use mapply to vectorise and return 10 results instead of 1 as above
mapply(sumsq, 1:10, 1:10, MoreArgs = list(x = x))

## the Vectorize() function fully vectorises a function
vsumsq <- Vectorize(sumsq, c("mu", "sigma"))
vsumsq(1:10, 1:10, x)

# Debugging
## mesage   : A generic notification/diagnostic message produced by the message() function; execution of the function
##            continues
## warning  : An indication that something is wrong but not necessarily fata; execution of the function continues.
##            Warnings are generated by the warning() function
## error    : An indication that a fatal problem has occured and execution f the function stops. Error are produced
##            by the stop() function
## condition: A generic concept for indicating that something unexpected has occured; programmers can create their 
##            own custom conditions if they want
log(-1)
printmessage <- function(x) {
  if (x > 0)
    print("x is greater than zero")
  else
    print("x is less than or equal to zero")
  invisible(x)
}
printmessage(1)
printmessage(NA)
printmessage2 <- function(x) {
  if (is.na(x))
    print("x is a missing value")
  else if (x > 0)
    print("x is greater than zero")
  else
    print("x is less than or equal to zero")
  invisible(x)
}
printmessage2(NA)
x <- log(c(-1, 2))
printmessage2(x)
printmessage3 <- function(x) {
  if (length(x) > 1L)
    stop("'x' has length > 1")
  if (is.na(x))
    print("x is a missing value")
  else if (x > 0)
    print("x is greater than zero")
  else
    print("x is less than or equal to zero")
  invisible(x)
}
printmessage3(1:2)
printmessage4 <- Vectorize(printmessage2)
out <- printmessage4(c(-1, 2))


# Debugging Tools in R
## traceback()  : prints out the function call stack after an error occurs; does nothing if there's no error
## debug()      : flags a function for "debug" mode which allows you to step through execution of a function 
##                one line at a time
## browser()    : suspends the execution of a function wherever it is called and puts the function in debug mode
## trace()      : allows you to insert debugging code into a function at specified places
## recover()    : allows you to modify the error behaviour so that you can browse the function call stack

# Using traceback()
## Shows you how many levels deep you were in the running code when the error occured
mean(x)
traceback()
lm(y ~ x)
traceback()

## eval() and call() test
IPI::ipi.cleanUp()
mydata <- as.data.frame(matrix(rnorm(1:100, 2, 1), nrow = 50, ncol = 2))
names(mydata) <- c("x", "y")
head(mydata)
lm((y * (z ^ 2)) ~ x, data = mydata, 1:100)
IPI::ipi.traceCall()
eval(ncl)
traceback()
z <- 2
eval(ncl)
lm((y * (z ^ 2)) ~ x, mydata, 1:100)
plot(eval(ncl))
plot(lm((y * (z ^ 2)) ~ x, mydata, 1:100))

## Also works with
IPI::ipi.cleanUp()
mydata <- as.data.frame(matrix(rnorm(1:100, 2, 1), nrow = 50, ncol = 2))
names(mydata) <- c("x", "y")
head(mydata)
lm(data = mydata, (y * (z ^ 2)) ~ x, 1:100)
IPI::ipi.traceCall()
eval(ncl)
traceback()
z <- 2
eval(ncl)
lm((y * (z ^ 2)) ~ x, mydata, 1:100)

## Also works with
IPI::ipi.cleanUp()
mydata <- as.data.frame(matrix(rnorm(1:100, 2, 1), nrow = 50, ncol = 2))
names(mydata) <- c("x", "y")
head(mydata)
lm(subset = 1:100, data = mydata, (y * (z ^ 2)) ~ x)
IPI::ipi.traceCall()
eval(ncl)
traceback()
z <- 2
eval(ncl)
lm((y * (z ^ 2)) ~ x, mydata, 1:100)

## Also works with 
IPI::ipi.cleanUp()
system.time((y * (z ^ 2)) / x)
IPI::ipi.traceCall()
eval(ncl)
traceback()
x <- 10
y <- 50
z <- 2
eval(ncl)
system.time((y * (z ^ 2)) / x)

## Also works with 
IPI::ipi.cleanUp()
print((y * (z ^ 2)) / x)
IPI::ipi.traceCall()
eval(ncl)
traceback()
x <- 10
y <- 50
z <- 2
eval(ncl)
print((y * (z ^ 2)) / x)

## Also works with 
IPI::ipi.cleanUp()
sprintf("%d", (y * (z ^ 2)) / x)
IPI::ipi.traceCall()
eval(ncl)
traceback()
x <- 10
y <- 50
z <- 2
eval(ncl)
sprintf("%d", (y * (z ^ 2)) / x)

## Using debug()
## takes a function as its first argument
## Flag the lm() function for debugging
debug(lm)
lm(y ~ x)

## Unflag the lm() function for debugging
undebug(lm)

## Using recover()
## Change default R error behaviour
defError <- getOption("error")
options(error = defError)
getOption("error")
options(error = recover)

## This code doesn't work
read.csv("nosucfile")

# Profiling R Code
## Using system.time()
## user time    : time charged to the CPU(s) for this expression
## elapsed time : 'wall clock' time, the amount of time that passes for you when running the code
## Elapsed time > user time
system.time({
  htmlPage <- readLines('http://localhost:4000/Locations Beginning with Ratho.html')
  })
htmltools::html_print(htmltools::HTML(htmlPage))

## Elapsed time < user time
## Elapsed time should be less the user time is computer able to split the work across multiple processors
hilbert <- function(n) {
  i <- 1:n
  1 / outer(i - 1, i, "+")
}
x <- hilbert(1000)
system.time(svd(x))

## Timing longer Expressions
system.time({
  n <- 1000
  r <- numeric(n)
  for (i in 1:n) {
    x <- rnorm(n)
    r[i] <- mean(x)
  }
})

# The R Profiler
## Turn on the profiler
Rprof()

## Turn off the profiler
Rprof(NULL)

Rprof()
sample.interval = 10000
lm(y ~ x)

## Using summaryRprof()
## by.total divides the time spent in each function by the total run time
## by.self  does the same as by.total but first subtracts out time spent in functions above the current function
##          in the call stack
summaryRprof()$by.total
summaryRprof()$by.self
summaryRprof()$sample.interval
summaryRprof()$sampling.time

## Rprof() runs the profiler for performance of analysis of R code
## summaryRprof() summarises the output of Rprof() and gives the percent of time spent in each function (with two
##  types of normalisation)

# Generating Randoms Numbers
## rnorm  : generate random Normal variates with a given mean and standard deviation
## dnorm  : evaluate the Normal probability density (with a given mean/SD) at a point (or vector of points)
## pnorm  : evaluate the cumulative distribution function for a Normal distribution
## rpois  : generate random Poisson variates with a given rate

## The four probability distributions starting with the following letters
## d for density
## r for random number generation 
## p for cumulative distribution
## q for quantile function ( inverse cumulative distribution)
dnorm(x, mean = 0, sd = 1, log = FALSE)
pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
rnorm(n, mean = 0, sd = 1)

## Simulate standard Normal random numbers
x <- rnorm(10)
x
x <- rnorm(10, 20, 2)
x
summary(x)
pnorm(2)

## Setting the random number seed
set.seed(1)
rnorm(5)
rnorm(5)

## Produces same as when set above 
## set.seed() should always be used when conducting a simulation
set.seed(1)
rnorm(5)

## Counts with a mean of 1
rpois(10, 1)

## Counts with a mean of 2
rpois(10, 2)

## Counts with a mean of 20
rpois(10, 20)

## Cumulative distribution
ppois(2, 2)
ppois(4, 2)
ppois(6, 2)

# Simulating a Linear Model
## Always set your seed
set.seed(20)

## Simulate predictor variable
x <- rnorm(100)

## Simulate the error term
e <- rnorm( 100, 0, 2)

## Compute the outcome via the model
y <- 0.5 + 2 * x + e
summary(y)
plot(x, y, main = 'plot of chunk Linear Model')

## Generate binary random variabes
set.seed(10)
x <- rbinom(100, 1, 0.5)

## x is now 0s and 1s
str(x)  
e <- rnorm(100, 0, 2)
y <- 0.5 + 2 * x + e
plot(x, y, main = 'plot of chunk Linear Model Binary')

## Simulate from a generalised Linear Model where the errors come from a Poisson log-linear model
set.seed(1)

## Simulate the predictor variable as before
x <- rnorm(100)

## Compute the log mean of the model and exponentiate it to get the mean to pass to rpois()
log.mu <- 0.5 + 0.3 * x
y <- rpois(100, exp(log.mu))
summary(y)
plot(x, y, main = 'plot of chunk Poisson Log-Linear Model')

# Random Sampling
set.seed(1)
sample(1:10, 4)
sample(1:10, 4)

## Doesn't have to be numbers
sample(letters, 5)

## Do a random permutation
sample(1:10)
sample(1:10)

## Sample w/replacement
sample(1:10, replace = TRUE)

## Sample indices of a data frame
library(datasets)
data(airquality)
?data
head(airquality)
set.seed(20)

## Create index vector
idx <- seq_len(nrow(airquality))

## Sample from the index vector
samp <- sample(idx, 6)
airquality[samp, ]

# Changes in Fine Particle Air Pollution in the U.S.
## Reading in the 1999 data
pm0 <- read.table(
  "~/Source Files/txt/RD_501_88101_1999-0.txt", 
  comment.char = "#", 
  header = FALSE, 
  sep = "|", 
  na.strings = ""
)
head(pm0[, 1:13])
cnames <- readLines("~/Source Files/txt/RD_501_88101_1999-0.txt", 1)
cnames <- strsplit(cnames, "|", fixed = TRUE)

## Ensure names are properly formatted
names(pm0) <- make.names(cnames[[1]])
head(pm0[, 1:13])
x0 <- pm0$Sample.Value
summary(x0)
mean(is.na(x0))

## Reading in the 2012 data
pm1 <- read.table(
  "~/Source Files/txt/RD_501_88101_2012-0.txt", 
  comment.char = "#", 
  header = FALSE, 
  sep = "|", 
  na.strings = "", 
  nrow = 1304290
)
nrow(pm1)
names(pm1) <- make.names(cnames[[1]])
x1 <- pm1$Sample.Value

## Entire U.S. analysis
boxplot(log2(x0), log2(x1), main = "plot of chunk boxplot log values")
axis(1, c(1, 2), c("1999", "2012"))
summary(x0)
summary(x1)
negative <- x1 < 0
mean(negative, na.rm = T)
dates <- pm1$Date
dates <- as.Date(as.character(dates), "%Y%m%d")
missing.months <- month.name[as.POSIXlt(dates)$mon + 1]
tab <- table(factor(missing.months, levels = month.name))
round(100 * tab / sum(tab))

## Changes in PM levels at an individual monitor
site0 <- unique(subset(pm0, State.Code == 36, c(County.Code, Site.ID)))
site1 <- unique(subset(pm1, State.Code == 36, c(County.Code, Site.ID)))
site0 <- paste(site0[, 1], site0[, 2], sep = ".")
site1 <- paste(site1[, 1], site1[, 2], sep = ".")
str(site0)
str(site1)
both <- intersect(site0, site1)
print(both)

## Find how many observations available at each monitor
pm0$county.site <- with(pm0, paste(County.Code, Site.ID, sep = "."))
pm1$county.site <- with(pm1, paste(County.Code, Site.ID, sep = "."))
cnt0 <- subset(pm0, State.Code == 36 & county.site %in% both)
cnt1 <- subset(pm1, State.Code == 36 & county.site %in% both)

## 1999
sapply(split(cnt0, cnt0$county.site), nrow)

## 2012
sapply(split(cnt1, cnt1$county.site), nrow)
both.county <- 63
both.id <- 2008

## Choose county 63 and site ID 2008
pm0sub <- subset(pm0, State.Code == 36 & County.Code == both.county & Site.ID == both.id)
pm1sub <- subset(pm1, State.Code == 36 & County.Code == both.county & Site.ID == both.id)

## Time Seires data
dates0 <- as.Date(as.character(pm0sub$Date), "%Y%m%d")
x0sub <- pm0sub$Sample.Value
dates1 <- as.Date(as.character(pm1sub$Date), "%Y%m%d")
x1sub <- pm1sub$Sample.Value

## Find global range
rng <- range(x0sub, x1sub, na.rm = T)
?par
par(mfrow = c(1, 2), mar = c(4, 5, 2, 1))
plot(dates0, x0sub, pch = 20, ylim = rng, xlab = "", 
     ylad = expression(PM[2.5] * " (" * mu * g / m ^ 3 * ")"
     ))
abline(h = median(x0sub, na.rm = T))
plot(dates1, x1sub, pch = 20, ylim = rng, xlab = "", 
     ylad = expression(PM[2.5] * " (" * mu * g / m ^ 3 * ")"
     ))
abline(h = median(x1sub, na.rm = T))

## Changes in state-wide PM levels
## 1999
mn0 <- with(pm0, tapply(Sample.Value, State.Code, mean, na.rm = TRUE))

## 2012
mn1 <- with(pm1, tapply(Sample.Value, State.Code, mean, na.rm = TRUE))

## Make separate data frames for states / years
d0 <- data.frame(state = names(mn0), mean = mn0)
d1 <- data.frame(state = names(mn1), mean = mn1)
mrg <- merge(d0, d1, by = "state")
head(mrg)

par(mfrow = c(1, 2))
rng <- range(mrg[, 2], mrg[, 3])
with(mrg, plot(rep(1, 52), mrg[, 2], xlim = c(.5, 2.5), ylim = rng, xaxt = "n", xlab = "", 
               ylab = "State-wide Mean PM", main = 'plot of chunk unnamed-chunk-13'
))
with(mrg, points(rep(2, 52), mrg[, 3]))
segments(rep(1, 52), mrg[, 2], rep(2, 52), mrg[, 3])
axis(1, c(1, 2), c("1999", "2012"))

