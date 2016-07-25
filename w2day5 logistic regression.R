# Logistic Regression
library("pROC")
library("stats")

speeddating.aggregated = read.csv("~/Documents/Dropbox/Stats Programming/Signal Data Science Nathan/speeddating-aggregated.csv")
speeddating.full = read.csv("~/Documents/Dropbox/Stats Programming/Signal Data Science Nathan/speeddating-full.txt")

df = speeddating.aggregated
df = na.omit(df)

# Using glm()

colnames(df)
trim_attr = function(df){
  subset_df = df[,c(1,2,7,8, 9:25)]
  return(subset_df)
}

df = trim_attr(df)
colnames(df)

# Predicting gender based on 17 activities

gender_logit = glm(gender~. - attr_o - race - career_c, family = "binomial", df)
summary(gender_logit)

#Predicting academia or business career based on 17 activities

academic_business = filter( df, df$career_c == 2 | df$career_c == 7)

academic_business$career_c = ifelse(academic_business$career_c == 2, 0, 1)
head(academic_business$career_c)

acad_buss_logit = glm(career_c ~. - attr_o - race - gender, family = "binomial", academic_business)
summary(acad_buss_logit)


# Predicting white or asian based on 17 activities
white_asian = filter( df, df$race == 2 | df$race == 4)
white_asian$race = ifelse(white_asian$race == 2, 0, 1)
whiteasian_logit = glm(race ~. - attr_o - career_c - gender, family = "binomial", white_asian)
summary(whiteasian_logit)

# Regularized linear regression
df_full = speeddating.full

# Helpful Tidbits to remind how to
# drop columns using a vector of boolean values

data <- data[,var.out.bool] # or...
data <- data[,var.out.bool, drop = FALSE] # You will need this option to avoid the conversion to an atomic vector if there is only one column left
var.out.bool <- !names(data) %in% c("iden", "name", "x_serv", "m_serv")
dtest <- data.frame(x=1:5, y=2:6, z = 3:7)
drop_vec <- c("x", "y")

null_assign <- function(df, names) {
  df[names] <- list(NULL)
  df
}

re_assign <- function(df, drop) {
  df <- df [, ! names(df) %in% drop, drop = FALSE]
  df
}