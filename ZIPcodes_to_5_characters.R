# Correcting ZIP codes to standard 5 digit format.
library(stringr) 
# trim all the area codes (the last 4 digits)
for (f in 1:length(df$X9..ZIP)) {
  if (nchar(df$X9..ZIP[f]) == 9) {
    df$X9..ZIP[f] <- str_sub(string = df$X9..ZIP[f], start = 1, end = 5)
  } else if (nchar(df$X9..ZIP[f]) == 8) {
    df$X9..ZIP[f] <- str_sub(string = df$X9..ZIP[f], start = 1, end = 4)
  }
}
## pad leading zero to ZIPs without 5 characters
df$ZIPcode <- str_pad(df$X9..ZIP, width=5, side="left", pad="0")

table(df$ZIPcode)
