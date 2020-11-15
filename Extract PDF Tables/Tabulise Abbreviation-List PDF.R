library(tabulizer)
library(dplyr)
location <- "C:\\Users\\Paul\\Documents\\Source Files\\pdf\\Abbreviation-List.pdf"
out <- extract_tables(location)
final <- do.call(rbind, out)
states <- rbind(as.matrix(final[2:31, 1:2], ncol=2), as.matrix(final[2:31, 3:4], ncol =2))
states <- states[states[,2] > "",]
colnames(states) <- as.data.frame(out)[1,1:2]
View(states)
directional <- as.matrix(final[34:41, 1:2], ncol=2)
colnames(directional) <- lapply(as.data.frame(out)[33,1:2], function(x) {gsub("\r", "\\\n", x)})
View(directional)
address <- as.matrix(final[34:41, 3:4], ncol =2)
address <- address[address[,2] > "",]
colnames(address) <- lapply(as.data.frame(out)[33,3:4], function(x) {gsub("\r", "\\\n", x)})
View(address)