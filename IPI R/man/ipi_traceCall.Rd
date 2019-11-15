\name{ipi.traceCall}
\alias{ipi.traceCall}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ipi.traceCall
}
\description{
Limited function that builds a callable expression from the Traceback call stack as long as the order of arguments match that of the original function.
}
\usage{
ipi.traceCall();
eval(ncl)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns an expression 'ncl' which can be used with the eval() function.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Paul I Ighofose
}
\note{
If the original function that failed causing the Traceback stack did not follow it's argument ordering then this may fail.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
ipi.traceCall()
eval(ncl)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
