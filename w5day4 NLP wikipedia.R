# Cleaning wikipedia data

# Load the necessary packages
library("tm")
library("qdap")

# Read in the data
df = read.csv("wikipedia_machinelearning_scrape.txt",
              header = FALSE, stringsAsFactors = FALSE)
dim(df)

# Verify that a given element is a paragraph or blank line
df[1,1]

# Check text for problems with check_text
problems_to_fix = check_text(df)
print(problems_to_fix, include.text=FALSE)

# Perform check_text's recommended cleaning procedures

# try with an sapply
df_cleaner = sapply(df, add_incomplete)
dim(df_cleaner)
str(df_cleaner)

# set the weird "|" items to NA
changetoNA <- function(colnum,df, to_replace) {
  col <- df[,colnum]
  col[col == to_replace] <- NA
  return(col)
}

df_cleaner2 <- data.frame(sapply(1:ncol(df_cleaner), changetoNA, df_cleaner, "|"))
dim(df_cleaner2)

# not working, and not certain I need it, so omitted for now
# df_cleaner = sapply(df_cleaner, enc2utf8)


df_cleaner = comma_spacer(df_cleaner)
df_cleaner = replace_number(df_cleaner)
df_cleaner = incomplete_replace(df_cleaner)
dim(df_cleaner)

# not run yet
spelling_check = check_spelling_interactive(df_cleaner)
?check_spelling_interactive
# problem running this one
df_cleaner = sentSplit(df_cleaner)


?Encoding


# Recheck text with check_text


# If clean, run the analysis


# Debug (via halving) if necessary to deal with any errors