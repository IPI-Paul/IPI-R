\name{ipi.getDateFormat}
\alias{ipi.getDateFormat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ipi.getDateFormat
}
\description{
Exports and then lists formats that can be used in date display functions
}
\usage{
ipi.getDateFormat(flPth)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flPth}{
Path to export to text file.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a list of format descriptors for use in date formatting
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
## Get date formats
ipi.getDateFormat(file.path(ipiSourceFiles,'txt','IPI Date Formats.txt'))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
