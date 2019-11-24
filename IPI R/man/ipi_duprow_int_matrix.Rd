\name{ipi.duprow.int.matrix}
\alias{ipi.duprow.int.matrix}
\alias{ipi.dup.int.matrix}
\alias{ipi.duprow.char.matrix}
\alias{ipi.dup.char.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ipi.duprow.int.matrix, ipi.dup.int.matrix, ipi.duprow.char.matrix and ipi.dup.char.matrix
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
The frist time either of these functions are run, there is a short delay will C++ loads. After that, there is no delay in response.
(duprow) Uses C++ and the Rcpp library to check integer or character matrices for duplicates and returns row numbers. The matrices will then return only the rows with the row numbering returned. \cr
The others are currently for single row matrices and also use C++ to check integer or character matrices for duplicates, but return TRUE on the 1st match. Therefore may not need to run a full list search.
}
\usage{
y[ipi.duprow.int.matrix(x, y),] \cr
y[ipi.duprow.char.matrix(x, y),] \cr
if(!ipi.duprow.int.matrix(x, y)) perform actions \cr
if(!ipi.duprow.char.matrix(x, y)) perform actions \cr

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
The matrix containing the data to search for as a duplicate and either filter by or meet ifelse condition.
}
  \item{y}{
%%     ~~Describe \code{x} here~~
The matrix containing the data being searched and either requiring filtering or validating ifelse condition.}
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
Please note, requires the Rcpp library.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
w <- ipi.permute("monday")

z <- w$NumericVectors[c(555:556, 120:130),]
w$NumericVectors[ipi.duprow.int.matrix(z, w$NumericVectors),]
nrow(w$NumericVectors[ipi.duprow.int.matrix(z, w$NumericVectors),])

z <- w$NumericVectors[555,]
if (ipi.dup.int.matrix(z, w$NumericVectors)) {print("Found duplicate in integer matrix")}

z <- w$Vectors[c(555:556, 120:130),]
w$Vectors[ipi.duprow.char.matrix(z, w$Vectors),]
nrow(w$Vectors[ipi.duprow.char.matrix(z, w$Vectors),])

z <- matrix(w$Permutations[555,], ncol = 6)
if (ipi.dup.char.matrix(z, w$Permutations)) {print("Found duplicate in character matrix")}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
