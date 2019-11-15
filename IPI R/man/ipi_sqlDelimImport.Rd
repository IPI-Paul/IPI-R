\name{ipi.sqlDelimImport}
\alias{ipi.sqlDelimImport}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ipi.sqlDelimImport
}
\description{
Function to import csv, tsv and other txt type files to SQL Server
}
\usage{
ipi.sqlDelimImport(svr, db, tbl, prms = list(...), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{svr}{
Your SQL Server Name. Use double backslashes to avoid character escape errors
}
  \item{db}{
You SQL Server database name
}
  \item{tbl}{
The SQL Server Table name to be created. Will cause an error if the Table already exists
}
  \item{prms}{
    list() to use deafult of all rows and tab delimited separator for reading in file.\cr
    Or include the following enclosed in a list():\cr
      cnames = c("Column1", "Column2", ...)\cr
      sep = "," or ";" or "\\t" or " "\cr
      nrows = 100
}
  \item{\dots}{
Change Column types giving Column name and type (e.g 'Column1 Name' = 'nvarchar(max)','Column2Name' = 'nvarchar(10000)')
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
## The function is currently defined as
function (svr, db, tbl, prms = list(...), ...)
{
    try(library(data.table))
    try(library(sqldf))
    try(library(odbc))
    if (length(prms) == 0)
        prms = list(a = 0)
    spr <- ifelse(!is.na(unlist(prms)["sep"]), prms$sep, "\t")
    if (is.na(unlist(prms)["nrows"])) {
        df <- fread(file.choose(), sep = spr, showProgress = TRUE)
    }
    else {
        df <- fread(file.choose(), sep = spr, nrows = prms$nrows,
            showProgress = TRUE)
    }
    ifelse(!is.na(unlist(prms)["cnames"]) | !is.na(unlist(prms)["cnames1"]),
        colnames(df) <- prms$cnames, "")
    con <- dbConnect(odbc(), Driver = "SQL Server Native Client 11.0",
        Server = svr, Database = db, trusted_connection = "yes")
    types <- list(...)
    tryCatch({
        dbWriteTable(conn = con, name = tbl, value = df, field.types = types)
    }, error = function(cond) {
        message(paste("If the column size is too big, then add the column name and column type as the last paramenter.e.g.",
            "ipi.sqlTSVImport(
            'YourSqlServer',
            'YourSqlDatabase',
            'YourSqlTable',
            list(
              cnames = c('ColumnNames'),    default: 1st row
              nrows = 'rows',               default: Read All
              sep = ','                     default: '\t'
            ),
            'Column1 Name' = 'nvarchar(max)',
            'Column2Name' = 'nvarchar(10000)',
            moreColumns....
          )",
          cond))
        return(NULL)
    })
  }

## import delimited file to SQL Server
ipi.sqlDelimImport('(local)', 'Studies', 'Temp', list(sep = ';', nrows = 100))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
