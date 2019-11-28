\name{ipi.function.load}
\alias{ipi.function.load}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
ipi.function.load
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Avoids loading all functions in a script file by loading only the function that meets the passed in function name. Currently required when testing numerous functions requiring functions in other scripts.
}
\usage{
ipi.function.load(fPth , func, envir = .GlobalEnv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fPth}{
%%     ~~Describe \code{x} here~~
The file path as a string that will enable R to find and parse the text of that file.
}
  \item{func}{
%%     ~~Describe \code{x} here~~
The function name as a string to load into the specified environment.
}
  \item{envir}{
%%     ~~Describe \code{x} here~~
The default is .GlobalEnv as this allows testing to not be delayed. You may specify specific environments should it be necessary.
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
Loads a single function from a script file.
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
ipi.function.load("~/Sub Folder of Documents/Your File Name.R", "Your Function Name")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
