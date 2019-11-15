\name{ipi.htmlPreview}
\alias{ipi.htmlPreview}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
ipi.htmlPreview
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Provides a file open dialog if no argument passed and prints the web page content to the RStudio Viewer.
}
\usage{
ipi.htmlPreview(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
The location of the web page. Can be on the localhost or the internet.
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
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
ipi.htmlPreview('http://localhost:4000/Locations Beginning with Ratho.html')
or
ipi.htmlPreview()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
