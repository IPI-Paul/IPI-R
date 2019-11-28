\name{Archive_PermTests.Rd}
\alias{Archive_PermTests.Rd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
ArchivePermTests
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
When testing numerous different methods to achieve permutaions in R that match the speed of those achieved in Python, the results were beginning to mount up and look like a scene out of Resident Evil. \cr
Various approaches in both logic and available packages were applied but still the speed rapidly diminishes with larger character permutations.\cr
However, much insight has been gained in to how to track errors; capture and combine generated reports with personalised reports; and Run batch testing on numerous functions at once.\cr
This file generates Console reporting whilst running as surce code and evetually generates a data.frame combining all results.\cr
The desired speed not yet achieved is 5 seconds on a 10 character input generating 3,628,800 rows.
}
\usage{
source("Archive_PermTests.R")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
%%  \item{x}{
%%     ~~Describe \code{x} here~~
%%}
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
After viewing a stackoverflow user's comment about the Rcpp::cppFunction being much faster, I gave it a try. It performed worse than everything so far. Although I have not seen any issues running the same function in the console with a for loop iteration to 720, running the cppfunction within another function 720 iterations causes R to crash. I got round this 5 character permutation limit by only running the code every nth character, but still the limit was a six character permutation. This was then improved upon and a more stable version is available as ipi.dup.rem()
}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# Ensure to load 'RunTests' in Archive_PermTests.R. Change the last line of the file or run direct from the console:
RunTests(sapply(3:5, function(i) paste0(letters[1:i], collapse = "")), c(1:23))
RunTests(sapply(6:6, function(i) paste0(letters[1:i], collapse = "")), c(1:23))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
