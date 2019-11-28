\name{ipi.duprow.int.matrix}
\alias{ipi.dup.rem}
\alias{ipi.duprow.int.matrix}
\alias{ipi.duprow.int.matrixi}
\alias{ipi.dup.int.matrix}
\alias{ipi.duprow.char.matrix}
\alias{ipi.duprow.char.matrixi}
\alias{ipi.dup.char.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ipi.dup.rem, ipi.duprow.int.matrix, ipi.dup.int.matrix, ipi.duprow.char.matrix and ipi.dup.char.matrix
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
The frist time either of these functions are run, there is a short delay while C++ loads. After that, there is no delay in response. \cr
ipi.dup.rem has been found to be more stable within other function iterated calls. The bgn argument indicates which rows to ignore as the data in those rows have already been checked for duplicates. Accepts either numeric or character vectors, matrices or data frames.\cr
(duprow) Uses C++ and the Rcpp library to check integer or character matrices for duplicates and returns row numbers for y or x where matrixi and x is not in y. The matrices will then return only the rows with the row numbering returned. \cr
The others are currently for single row matrices and also use C++ to check integer or character matrices for duplicates, but return TRUE on the 1st match. Therefore may not need to run a full list search.
}
\usage{
rem <- ipi.dup.rem(data, bgn, ncol) \cr
y[ipi.duprow.int.matrix(x, y, z),] \cr
x[ipi.duprow.int.matrixi(x, y, z),] \cr
y[ipi.duprow.char.matrix(x, y, z),] \cr
x[ipi.duprow.char.matrixi(x, y, z),] \cr
if(!ipi.duprow.int.matrix(x, y, z)) perform actions \cr
if(!ipi.duprow.char.matrix(x, y, z)) perform actions \cr

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, data }{
%%     ~~Describe \code{x} here~~
The matrix containing the data to search for as a duplicate and either filter by or meet ifelse condition.
}
  \item{y, bgn }{
%%     ~~Describe \code{x} here~~
When y, the matrix containing the data being searched and either requiring filtering or validating ifelse condition. And when bgn, The row number to stop at as previous searches will have already cheked rows below.
}
  \item{z, ncol }{
%%     ~~Describe \code{x} here~~
The number of columns for the matrices/matrix.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Either TRUE, FALSE, the rows in x/data that are duplicates or when matrixi those that aren't.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Paul I Ighofose
}
\note{
%%  ~~further notes~~
Please note, requires the Rcpp library. Due to temprement it is also required to provide the number of columns initially.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
library(IPI)
w <- ipi.permute("monday")

z <- matrix(as.matrix(rbind(w$NumericVectors, w$NumericVectors[c(100:150, 350:400),])), ncol = ncol(w$NumericVectors))
cat("Total Rows:", nrow(z), "  Unique Rows:", nrow(unique(z)), "\n")
bgn <- 0
rows <- nrow(w$NumericVectors)
while (nrow(z) > rows){
  rem <- ipi.dup.rem(z, bgn, ncol = ncol(z))
  if(sum(rem) > 0) {
    z <- matrix(z[-rem,], ncol = ncol(z))
    bgn <- nrow(z)
  } else {
    break
  }
}
cat("Total Rows:", nrow(z), "  Unique Rows:", nrow(unique(z)), "\n")

z <- w$NumericVectors[c(555:556, 120:130),]
w$NumericVectors[ipi.duprow.int.matrix(z, w$NumericVectors, 6),]
nrow(w$NumericVectors[ipi.duprow.int.matrix(z, w$NumericVectors, 6),])

z[ipi.duprow.int.matrix(z, w$NumericVectors, 6),]
nrow(z[ipi.duprow.int.matrix(z, w$NumericVectors, 6),])

z <- w$NumericVectors[555,]
if (ipi.dup.int.matrix(z, w$NumericVectors, 6)) {print("Found duplicate in integer matrix")}

z <- w$Vectors[c(555:556, 120:130),]
w$Vectors[ipi.duprow.char.matrix(z, w$Vectors, 6),]
nrow(w$Vectors[ipi.duprow.char.matrix(z, w$Vectors, 6),])

z[ipi.duprow.char.matrix(z, w$Vectors, 6),]
nrow(z[ipi.duprow.char.matrix(z, w$Vectors, 6),])

z <- matrix(w$Permutations[555,], ncol = 6)
if (ipi.dup.char.matrix(z, w$Permutations, 6)) {print("Found duplicate in character matrix")}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
