# Principle Component Analysis

library("psych")
library("corrplot")
library("DAAG")
df = select(msq, active:scornful)

sort(colSums(is.na(df)))
drops = c('alone', 'kindly', 'scornful', 'tranquil', 'inactive', 'idle', 'anxious', 'cheerful')
df[drops] = NULL
extrav_df = df
neurot_df = df
extrav_df$Extraversion = msq$Extraversion
neurot_df$Neuroticism = msq$Neuroticism
extrav_df = na.omit(extrav_df)
neurot_df = na.omit(neurot_df)

df = na.omit(df)
str(prcomp(df))
head(prcomp(df)[3])
View(df)
p = prcomp(df, scale. = TRUE)
p$sdev
p$rotation
p$x
top = function(n, df){
  # Prints out top ten loadings of nth PrinComp
  # ordered by absolute value
  PCA = prcomp(df, scale. = TRUE)$rotation[,n]
  PCA = PCA[order(abs(PCA), decreasing = TRUE)]
  return(PCA[1:20])
}

PCnames = c('lively_to_not','upset_to_fine','calm_to_jittery','sleepy_to_awake','scared_to_uncomfortable')
topFive = c()
for(i in 1:5){
  print(c("PC #", PCnames[i]))
  print(top(i, df))
  topFive[i] = top(i, df)
}
topFive = data.frame(topFive)
colnames(topFive) = PCnames
# Corrplot 1:5 or 1:10 prcomp$rotation (PCA loadings)
corrplot(p$rotation[,1:5], is.corr = FALSE)

# Eigenvalue Plot
eigenvals = data.frame(seq_along(p$sdev), p$sdev)
ggplot(data = eigenvals) + geom_point(aes(x=eigenvals[1],y=eigenvals[2]))
ggplot(data = eigenvals) + geom_point(aes(x=eigenvals[1],y=eigenvals[2])) + xlim(0,10)

extrav_PCA = prcomp(scale(extrav_df[,colnames(extrav_df) != 'Extraversion']))
extrav_PCs = data.frame(extrav_df["Extraversion"],extrav_PCA$x)
colnames(extrav_PCs)
names(extrav_PCs)


fit_extrav = lm(Extraversion~., data = extrav_PCs)
fit_neurot = lm(Neuroticism~., data = neurot_df)


predict_extrav = predict(fit_extrav, newdata = extrav_PCs[,colnames(extrav_PCs) != 'Extraversion'])
?predict.lm

k_progression = function(df, target){
  feature_df = df[,colnames(df) != target]
  lm_formula = as.formula(paste0(target, "~."))
  rmse_vector = c(rep(NA, ncol(feature_df)))

  for(i in 1:ncol(feature_df)){
    fit = CVlm(data = df[1:(i+1)], form.lm =lm(lm_formula, data = df[1:(i+1)]), m = 10, plotit = FALSE)
    #prediction = predict(fit, newdata = feature_df[1:i])
    #print(head(df[target]))
    rmse_vector[i] = rmse(fit$cvpred, df[target])

  }
  return(rmse_vector)
}

rmse_extraversion = k_progression(extrav_PCs, "Extraversion")
rmse_extraversion

rmse = function(x,y){
  # root-mean-square-error

  numerator = sum((x-y)^2)
  denom = length(x)
  return(sqrt(numerator/denom))
}

rmse_extrav = rmse(predict_extrav, extrav_df$Extraversion)

View(extrav_df)




spddf = read.csv('speeddating-aggregated.csv')
spddf = na.omit(spddf)
activities = select(spddf, sports:yoga)
q = prcomp(activities, scale.=TRUE)
q$rotation
qcors = corrplot(q$rotation, is.corr=FALSE)
qeigenvals = data.frame(seq_along(q$sdev), q$sdev)
ggplot(data=qeigenvals) +
  geom_point(aes(qeigenvals[1],qeigenvals[2]))

q2 = cbind(activities, q$x)
corrplot(cor(q2))

klogprogression = function(feature_df, target){
  df = cbind(target, feature_df)
  colnames(ta)
  lm_formula = as.formula(paste0(target, "~."))
  pval_vector = numeric(length=ncol(feature_df))
  coef_vector = numeric(length=ncol(feature_df))
  for(i in 1:ncol(feature_df)){
    fit = glm(data = df[1:(i+1)], formula=lm_formula, family="binomial")
    #prediction = predict(fit, newdata = feature_df[1:i])
    #print(head(df[target]))
    pval_vector[i] = list(coef(summary(fit))[2:ncol(df),4])
    coef_vector[i] = list(coef(fit)[2:ncol(df)])
  }
  return(c(pval_vector, coef_vector))
}
test_df = cbind(spddf$gender,q$x)

typeof(test_df)
colnames(test_df)[1] = 'gender'

fit = glm(data=data.frame(test_df), formula=gender~., family='binomial')
str(fit)
coef(summary(fit))[2:18,4]
coef(fit)
