\name{ipi.permute}
\alias{ipi.permute}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ipi.permute
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
If each letter is not unique generates 4 data frames and 2 total variables of permutations from given word or words. Otherwise, generates a single total variable and 3 data frames of the permutations their indexed positions and each character split into columns.
}
\usage{
ipi.permute(s)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{
%%     ~~Describe \code{s} here~~
The character string of word or words to permute
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{Total }{Total number of possible unique permutations}
  \item{NonUniqueTotal }{Total number of possible permutations}
  \item{Permutations }{A data frame containing a unique list of permutations.}
  \item{NonUniquePermutations }{A data frame containing a list of all permutations.}
  \item{Vectors }{A data frame containing a list of all character permutations in columns.}
  \item{NumericVectors }{A data frame containing a list of all indexed permutations in columns.}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Paul I Ighofose
}
\note{
%%  ~~further notes~~
Please note that permutations over 10 will increase in processing times expotentially.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
w <- ipi.permute("Monday")
w$Total
w$Permutations
w$Vectors
w$NumericVectors

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
