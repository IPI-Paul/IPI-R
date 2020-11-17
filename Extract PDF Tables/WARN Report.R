library(tabulizer)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  require(rstudioapi)
  sPath <- dirname(getActiveDocumentContext()$path)
  fPath <- file.choose()
} else {
  sPath <- args[1]
  fPath <- args[2]
}
location <- file.path(fPath)
out <- extract_tables(location)
final <- do.call(rbind, out[1:230])[-1,]
final[,4] <- sapply(final[,4], function(x) {
  x <- gsub("&ldquo;", "\"\"", x)
  x <- gsub("&rdquo;", "\"\"", x)
  x <- gsub("&amp;", "&", x)
  x <- trimws(x, "right", "\r")
  x <- gsub("-\\r", "-", x)
  x <- gsub("\\r", " ", x)
  x <- trimws(x, "right")
  })
final[,1:3] <- format(as.Date(final[,1:3], "%m/%d/%Y"), "%d/%m/%Y")
colnames(final) <- as.data.frame(out[1])[1,]
write.csv(final, file.path(sPath, "..", "csv", "WARN Report.csv"), row.names = FALSE, na = "")
