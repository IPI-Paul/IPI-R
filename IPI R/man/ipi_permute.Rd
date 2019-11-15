\name{ipi.permute}
\alias{ipi.permute}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ipi.permute
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
If each letter is not unique generates 3 data frames and 3 total variables of permutations from given word or words. Otherwise, generates a single total variable and a data frame of the permutations.
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
  \item{UniqueTotal }{Total number of possible unique permutations on unique characters}
  \item{Permutations }{A data frame containing a unique list of permutations.}
  \item{NonUniquePermutations }{A data frame containing a list of all permutations.}
  \item{UniquePermutations }{A data frame containing a unique list of permutations taken from unique characters of the word or words given.}
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
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- ipi.permute("Monday")
x$Total
x$Permutations
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
