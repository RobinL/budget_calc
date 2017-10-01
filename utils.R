options(scipen=999)
fmtc <- function(num) {
  paste("£",format(round(num,0), big.mark=","),sep="")
}