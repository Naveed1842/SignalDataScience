library(dplyr)
library(glmnet)
library(pROC)
library(dummies)


df =  read.csv("~/Documents/Dropbox/Stats Programming/Signal Data Science Nathan/speeddating-full.csv")

#Create data frame with decisions, average decision frequencies, careers and races
df = select(df, gender, iid, pid, wave, dec, attr, race, career_c)
genders = c("female", "male")
df$gender = factor(df$gender, labels = genders)
careers = c("Lawyer",
            "Academic",
            "Psychologist",
            "Doctor",
            "Engineer",
            "Creative",
            "Business",
            "RealEstate",
            "IntRelations",
            "Undecided",
            "SocialWork",
            "Speech",
            "Politics",
            "Athletics",
            "Other",
            "Journalism",
            "Architecture")
races = c("Black", "White", "Latino", "Asian", "Other")

# df$gender = factor(df$gender, labels = genders)
# df$race = factor(df$race, labels = races)
# df$career_c = factor(df$career_c, labels = careers)
agged = aggregate(df["dec"], df["iid"], FUN = mean, na.rm = T)

colnames(agged) = c("iid", "decAvg")
df = inner_join(df, agged)
agged = aggregate(df[c("dec", "attr")], df["pid"], FUN = mean, na.rm = T)
colnames(agged) = c("pid", "decPartnerAvg", "attrPartnerAvg")
df = inner_join(df, agged)
agged = aggregate(df[c("race", "career_c")], df["iid"], FUN = mean)
agged$race = factor(agged$race, labels = races)
agged$career_c = factor(agged$career_c, labels = careers)
names(agged)
df = inner_join(df[!(names(df) %in% c("race", "career_c"))], agged)
colnames(agged)[1:3] = c("pid", "race_Partner", "career_c_Partner")
df = inner_join(df, agged)



#Cross validate regularized logistic regression at the level of waves

crossValidate = function(features,
                         target,
                         waves = df$wave,
                         lambdas = (1.2)^(10:(-30)),
                         alphas = seq(0, 0.24, 0.03)){
  s = scale(features)
  s = s[,!is.nan(colSums(s))]
  rocs = expand.grid(lambda = lambdas, alpha = alphas)
  rocs$logLoss = 0
  rocs$ROC = 0
  for(alpha in alphas){
    print(alpha)
    l = lapply(1:21, function(wave){
      trainFeatures = s[waves != wave,]
      testFeatures = s[waves == wave,]
      set.seed(1); m = glmnet(trainFeatures, target[waves != wave],
                              alpha = alpha,
                              lambda = lambdas,
                              family = "binomial")
      as.data.frame(predict(m, testFeatures))
    })
    predictions = do.call(rbind, l)
    predictions = exp(predictions/(1 + predictions))
    rocTemp = sapply(predictions, function(cv){
      as.numeric(roc(target,cv)$auc)
    })
    rocs[rocs$alpha == alpha,"ROC"] = rocTemp[length(rocTemp):1]
  }
  rocs
}

colnames(df)


dums1 = dummy.data.frame(data =df[,c('race', 'career_c')])

dums2 = dummy.data.frame(data=df[c('race_Partner', 'career_c_Partner')])

dums = cbind(dums1, dums2)

dums = dums[,colSums(dums) > 20]



#function for creating new interaction columns and names
race_locs = setdiff(grep("race", colnames(dums)), grep("race_Partner", colnames(dums)))
career_c_locs = setdiff(grep("career_c", colnames(dums)), grep("career_c_Partner", colnames(dums)))
race_partner_locs = grep("race_Partner", colnames(dums))
career_c_partner = grep("career_c_Partner", colnames(dums))

interaction_maker = function(locations1, locations2, df){
  for(loc1 in locations1){
    for(loc2 in locations2){
      temp_name1 = colnames(df)[loc1]
      temp_name2 = colnames(df)[loc2]
      temp_name = paste(temp_name1, temp_name2, sep=":")
      df[temp_name] = df[,loc1] * df[,loc2]
    }

  }
  return(df)
}




dums = cbind(dums, df['attrPartnerAvg'])
dums = interaction_maker(length(colnames(dums)), race_locs, dums)
colnames(dums)
dums$attrPartnerAvg = NULL

dums = cbind(dums, df['attrPartnerAvg'])
dums = interaction_maker(length(colnames(dums)), career_c_locs, dums)
colnames(dums)
dums$attrPartnerAvg = NULL

dums = interaction_maker(race_locs, race_partner_locs, dums)
dums = interaction_maker(career_c_locs, career_c_partner, dums)

dums = dums[,colSums(dums) > 20]

features = cbind(dums, df$decAvg,df$decPartnerAvg, df$attrPartnerAvg)
results = crossValidate(features, df$dec)

features_male = features[df$gender == "male",]
features_female = features[df$gender == "female",]
dim(features_male)
results_male = crossValidate(features_male, df$dec[df$gender == 'male'], waves = df$wave[df$gender == 'male'])

bestTune = results_male[results_male['ROC'] == max(results_male['ROC']),]
bestTune
bestModel_male = glmnet(x = features_male, y=)
multinom_career = glmnet()
?'%in%'

probabilities = function(preds, rownum) {
  # preds must be a matrix of predictions
  sumLogOdds = 0
  probs = c()
  for(LogOdd in preds[rownum,]){
    sumLogOdds = sumLogOdds + exp(LogOdd)
  }
  for(i in 1:length(preds[rownum,])){
    probs[i] = exp(preds[i,rownum])/sumLogOdds
  }
  if(sum(probs) != 1) stop("Error in function: probabilties do not sum to 1.")
  return(probs)
}