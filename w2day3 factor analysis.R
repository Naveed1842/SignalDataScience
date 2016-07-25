# Factor Analysis
library("psych")
library("corrplot")
library("DAAG")


set.seed(1); factors = data.frame(x=rnorm(n=100),y=rnorm(n=100),z=rnorm(n=100))


noisyProxies = function(feature, k, correlation){
  if(sum(is.na(feature)) > 0) stop("There are NAs in your data.")
  set.seed(4)
  noisy_proxy_matrix = matrix(data= NA, ncol = k, nrow = length(feature))
  for(i in 1:k){
    for(j in 1:length(feature)){
    error = rnorm(1) * sqrt(1-correlation^2)
    noisy_proxy_matrix[j,i] = sum(correlation * feature[j]) + error
    }
  }
  return(data.frame(noisy_proxy_matrix))
}

XnoisyProxies = noisyProxies(factors$x, k=4, correlation = 0.9)
YnoisyProxies = noisyProxies(factors$y, k=3, correlation = 0.9)


noisies = data.frame(XnoisyProxies, YnoisyProxies)
colnames(noisies)[5:7]=c('Y1', 'Y2', 'Y3')
str(noisies)
names(noisies)

corrplot(as.matrix(noisies), is.corr = FALSE)

# Principle Component Analysis
p_factors = prcomp(factors)
p_compare = cbind(noisies, p_factors[['x']])
corrplot(cor(as.matrix(p_compare)), is.corr = FALSE)

# Generate new data with 50 entangled variables for Factor analysis
factors2_matrix = matrix(NA, ncol = 50, nrow = 100)
set.seed(1)
for(i in 1:50){
  factors2_matrix[,i] = (factors$x * runif(1)) + (factors$y * runif(1)) + (factors$z * runif(1)) + (0.5 * rnorm(length(factors$x)))
}
factors2 = data.frame(factors2_matrix)


# Run PCA on the new variables
p_factors2 = prcomp(factors2)
p_PCs = p_factors2$x[,1:3]
p_compare = cbind(p_PCs, factors)
corrplot(cor(as.matrix(p_compare)), is.corr = FALSE)

# Run Factor analysis on the new variables

# Orthogonal (varimax) factor Analysis -
fa_factors = fa(factors2, nfactors = 3, rotate = "varimax")
fa_compare = cbind(factors,fa_factors$scores)
corrplot(cor(as.matrix(fa_compare)), is.corr = FALSE, title = "Orthogonal")

# Oblique (oblimin) factor analysis
fa_factors = fa(factors2, nfactors = 3, rotate = "oblimin")
fa_compare = cbind(factors,fa_factors$scores)
corrplot(cor(as.matrix(fa_compare)), is.corr = FALSE, title = "Oblique")

# For Oblique Analysis, make entangled variable (W) to test
W = 0.5*factors$x + factors$y
W_compare = data.frame(cbind(W, factors$x))
ggplot(W_compare) + geom_point(aes(x=W_compare[1], y=W_compare[2])) + geom_smooth(aes(x=W_compare[1], y=W_compare[2]), color="red")
corrplot((as.matrix(W_compare)), is.corr = FALSE)

W_noisies = data.frame(noisyProxies(factors$x, 10,correlation = 0.8), noisyProxies(W, 4, correlation = 0.8))

colnames(W_noisies)[11:14] = c("W1", "W2", "W3", "W4")

# Orthogonal (varimax) factor Analysis - on W_noisies
# (cleaner, sends off-factor correlations to zero)
fa_factors = fa(W_noisies, nfactors = 2, rotate = "varimax")
fa_compare = cbind(factors,fa_factors$scores)
corrplot(cor(as.matrix(fa_compare)), is.corr = FALSE, title = "Orthogonal")

# Oblique (oblimin) factor analysis - on W_noisies
# (accurately detects entanglement, allows off-factor correlations)
fa_factors = fa(W_noisies, nfactors = 2, rotate = "oblimin")
fa_compare = cbind(factors,fa_factors$scores)
corrplot(cor(as.matrix(fa_compare)), is.corr = FALSE, title = "Oblique")



# Factor Analysis on Speed Dating dataset
spd_df = read.csv("speeddating-aggregated.csv")
activities = select(spd_df, sports:yoga)
x11()
spd_fa1 = fa(activities, nfactors = 4, rotate = "oblimin")
p1 = corrplot(spd_fa1$loadings, title = "Oblique")
x11()
spd_fa2 = fa(activities, nfactors = 4, rotate = "varimax")
p2 = corrplot(spd_fa2$loadings, title = "Orthogonal")

# Big Five personality data analysis

bf_df = read.table("big5_data.csv", sep = "\t", header = TRUE)
big5_features = select(bf_df, E1:O10)
big5_PCA = prcomp(big5_features, scale. = TRUE)
big5_fa = fa(big5_features, nfactors = 5, rotate = "varimax")
corrplot(cor(cbind(big5_features, big5_PCA$x, big5_fa$scores)), is.corr = FALSE)
corrplot(cor(cbind(big5_features, big5_fa$scores)), is.corr = FALSE)
?corrplot

# Comparing Logistic Regression Predictive Models for all vs factors

gender = bf_df$gender - 1
gender_attr = cbind(gender, big5_features)
gender_attr = filter(gender_attr, gender != -1 & gender != 2)

big5_glm_fit = glm(gender~., data = gender_attr, family = "binomial")

gender_attr_factor = fa(select(gender_attr, -gender), nfactors = 5, rotate = "varimax")
gender_attr_factor2 = data.frame(cbind(gender_attr$gender, gender_attr_factor$scores))
colnames(gender_attr_factor2)[1] = "gender"
big5_glm_factors_fit = glm(gender~., data = gender_attr_factor2, family = "binomial")

coef(big5_glm_fit)
coef(big5_glm_factors_fit)

corrplot(as.matrix(cor(c(coef(big5_glm_fit), coef(big5_glm_factors_fit)))))


colnames(big5_features)






section_fa = function(df, section_start, section_end){
  pos1 = match(section_start, colnames(df))
  pos2 = match(section_end, colnames(df))
  factor_set = df[pos1:pos2]
  #floor(ncol(factor_set)/2)
  factor_result = fa(factor_set, nfactors = 2, rotate="varimax")
  corrplot(cor(cbind(factor_set, factor_result$scores)), is.corr = FALSE, title = c(" "," ", "Orthogonal Factor Analysis Correlation", paste(section_start, section_end, sep = ":")))
  return(factor_result)
}



# Looking at just conscientiousness
big5_c_fa = section_fa(bf_df, "C1", "C10")

# Looking at just extraversion
big5_e_fa = section_fa(bf_df, "E1", "E10")

# Looking at just neuroticism
big5_n_fa = section_fa(bf_df, "N1", "N10")

# Looking at just agreeableness
big5_a_fa = section_fa(bf_df, "A1", "A10")

# Looking at just openness/intellect
big5_o_fa = section_fa(bf_df, "O1", "O10")





