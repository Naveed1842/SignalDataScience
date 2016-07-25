# Script for processing Zillow scrape

scrape_zillow = read.csv("/Users/nathanhelm-burger/herokufolder/flask\ tutorial/scrape_zillow.csv")
df = scrape_zillow
dim(df)


changetoNA <- function(colnum,df) {
  col <- df[,colnum]
  if (is.numeric(col)) {  #edit: verifying column is numeric
    col[col < 0 & is.numeric(col)] <- NA
  }
  return(col)
}

df <- data.frame(sapply(1:5, changetoNA, df))
df = cbind(df, scrape_zillow$url)
names(df) = names(scrape_zillow)
df$ppft[df$ppft>500000] = NA
boxplot(df$ppft)
summary(df)

