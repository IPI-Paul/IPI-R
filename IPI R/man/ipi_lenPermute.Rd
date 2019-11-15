\name{ipi.lenPermute}
\alias{ipi.lenPermute}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ipi.lenPermute
}
\description{
Calculates the total number of posible permutations from a number indicating the length of a word or words
}
\usage{
ipi.lenPermute(n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
The length of a word to permute
}
}
\details{
Used to confirm ipi.permute result set
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Total number of posible permutations
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Paul I Ighofose
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
ipi.lenPermute(4)
ipi.lenPermute(nchar("Monday"))


## The function is currently defined as
function (n)
{
    Vectorize(x <- function(j) {
        for (i in 1:(j - 2)) {
            j <- j + j * i
        }
        j
    })(n)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
