<<<<<<< HEAD
\name{ipi.colSum}
\alias{ipi.colSum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ipi.colSum
}
\description{
Aggregate data to give sum of specified columns
}
\usage{
ipi.colSum(inp, cols, hdrs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inp}{
use data.frame(vector1, Vector2) to pass in the vectors to be summed
}
  \item{cols}{
use list(Vector1, Vector2, Vector3, Vector4) for the vectors to be grouped by. Column naming of the summed columns looks for similar names in both inp and cols entries
}
  \item{hdrs}{
use c(ColName1, ColName2, ColName3, ColName4) to rename the columns
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{RecordSet}{
  returns a data.frame with named fields and summing as specified
  }
  \item{Status}{
  returns TRUE if the resulting totals match the source data totals
  }
  \item{Totals}{
  returns the totals of each Summed Vector
  }
}
\author{
Paul I Ighofose
}
\note{
Sum Of columns may not generate names if the names in the inp do not exist in cols
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
## The function is currently defined as
function (inp, cols, hdrs)
{
    newGroup <- as.data.frame(cols)
    newGroup <- aggregate(inp, sum, by = newGroup)
    sel <- as.table(match(inp, cols))
    names(sel) <- colnames(inp)
    names(newGroup) <- c(hdrs, as.list(paste0("sum of ", colnames(inp))))
    newGroup <- newGroup[order(newGroup[, 1]), ]
    status <- sum(inp) == sum(newGroup[paste0("sum of ", names(sel))])
    totals <- data.frame(rbind(1:length(colnames(inp))))
    names(totals) <- colnames(inp)
    for (i in colnames(inp)) {
        totals[i] <- sum(newGroup[[paste0("sum of ", names(sel[i]))]])
    }
    res <- list(newGroup, status, totals)
    names(res) <- c("RecordSet", "Status", "Totals")
    res
  }

## Aggregate Columns
heights <- round(runif(20, 1, 6))
genders <- c(rep("Male", 4), rep("Female", 5), rep("Male", 6), rep("Female", 5))
group <- ifelse(heights >= 3, "Free", "Paid")
charge <- ifelse(heights < 3, 1, ifelse(heights < 4, 2.5, 4))
weights <- round(runif(20, 1, 6))
dt <- ipi.colSum(data.frame(heights, charge),list(heights, genders, group, charge), c("Height", "Gender", "Group", "Charge"))
dt$RecordSet
dt$Status
dt$Totals
View(dt$RecordSet)
dt1 <- ipi.colSum(data.frame(heights, weights, charge),list(heights, weights, genders, group, charge), c("Height", "Weight", "Gender", "Group", "Charge"))
dt1$RecordSet
dt1$Status
dt1$Totals
View(dt1$RecordSet)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
=======
\name{ipi.colSum}
\alias{ipi.colSum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ipi.colSum
}
\description{
Aggregate data to give sum of specified columns
}
\usage{
ipi.colSum(inp, cols, hdrs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inp}{
use data.frame(vector1, Vector2) to pass in the vectors to be summed
}
  \item{cols}{
use list(Vector1, Vector2, Vector3, Vector4) for the vectors to be grouped by. Column naming of the summed columns looks for similar names in both inp and cols entries
}
  \item{hdrs}{
use c(ColName1, ColName2, ColName3, ColName4) to rename the columns
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{RecordSet}{
  returns a data.frame with named fields and summing as specified
  }
  \item{Status}{
  returns TRUE if the resulting totals match the source data totals
  }
  \item{Totals}{
  returns the totals of each Summed Vector
  }
}
\author{
Paul I Ighofose
}
\note{
Sum Of columns may not generate names if the names in the inp do not exist in cols
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
## The function is currently defined as
function (inp, cols, hdrs)
{
    newGroup <- as.data.frame(cols)
    newGroup <- aggregate(inp, sum, by = newGroup)
    sel <- as.table(match(inp, cols))
    names(sel) <- colnames(inp)
    names(newGroup) <- c(hdrs, as.list(paste0("sum of ", colnames(inp))))
    newGroup <- newGroup[order(newGroup[, 1]), ]
    status <- sum(inp) == sum(newGroup[paste0("sum of ", names(sel))])
    totals <- data.frame(rbind(1:length(colnames(inp))))
    names(totals) <- colnames(inp)
    for (i in colnames(inp)) {
        totals[i] <- sum(newGroup[[paste0("sum of ", names(sel[i]))]])
    }
    res <- list(newGroup, status, totals)
    names(res) <- c("RecordSet", "Status", "Totals")
    res
  }

## Aggregate Columns
heights <- round(runif(20, 1, 6))
genders <- c(rep("Male", 4), rep("Female", 5), rep("Male", 6), rep("Female", 5))
group <- ifelse(heights >= 3, "Free", "Paid")
charge <- ifelse(heights < 3, 1, ifelse(heights < 4, 2.5, 4))
weights <- round(runif(20, 1, 6))
dt <- ipi.colSum(data.frame(heights, charge),list(heights, genders, group, charge), c("Height", "Gender", "Group", "Charge"))
dt$RecordSet
dt$Status
dt$Totals
View(dt$RecordSet)
dt1 <- ipi.colSum(data.frame(heights, weights, charge),list(heights, weights, genders, group, charge), c("Height", "Weight", "Gender", "Group", "Charge"))
dt1$RecordSet
dt1$Status
dt1$Totals
View(dt1$RecordSet)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
>>>>>>> 5863e6f785c1b85928f7351c64dd57201babf032
