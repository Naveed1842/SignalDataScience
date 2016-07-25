df = read.csv('speeddating-aggregated.csv')
sum(complete.cases(df))
nrow(df)
df = na.omit(df)
a = glm(formula=gender~., family='binomial', data=df)
apred = predict(a, df[,2:ncol(df)])
str(apred)
aprobs = exp(apred)/(1 + exp(apred))

dfb = filter(df, career_c == 2 | career_c == 7)
dfb = select(dfb, 8:ncol(dfb))

dfb[[1]] = gsub(2,0,dfb[[1]])
dfb[[1]] = gsub(7,1,dfb[[1]])
b = glm(formula = as.numeric(career_c)~., data=dfb, family='binomial')
bpred = predict(b, dfb[,-1])
bprobs = exp(bpred) / 1 + exp(bpred)
roc(as.numeric(dfb[[1]]), as.numeric(bprobs))



dfc = filter(df, race == 2 | race == 4)
dfc = select(dfc, c(7,9:ncol(dfc)))
dfc[[1]] = as.numeric(gsub(2,0,dfc[[1]]))
dfc[[1]] = as.numeric(gsub(4,1,dfc[[1]]))
c = glm(formula = race~., data=dfc, family='binomial')
cpred = predict(c, dfc[-1])
cprobs = exp(cpred) / 1 + exp(cpred)
roc(dfc[[1]], as.numeric(cprobs))




# regularized linear regression
library(dplyr)
library(glmnet)
library(pROC)
library(dummies)

# df = select(df, gender, iid, pid, wave, dec, attr, race, career_c)

dums1 = dummy.data.frame(df[c("race", "career_c")])
dums2 = dummy.data.frame(df[c('race_Partner', 'career_c_Partner')])
dums = cbind(dums1, dums2)

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

# a = dums1[,8:11]*df[,9]




names(a) = lapply(names(a), function(x) paste(x,'attrPartnerAvg', sep = ":"))
dums = cbind(dums, a)
b = dums1[,7:24]*df[,9]
names(b) = lapply(names(b), function(x) paste(x,'attrPartnerAvg', sep = ":"))
dums = cbind(dums, b)
cnames1 = names(dums1[,1:6])
cnames2 = names(dums2[,1:6])

dums = cbind(dums, df['attrPartnerAvg'])
dums = interaction_maker(length(colnames(dums)), race_locs, dums)
colnames(dums)
dums$attrPartnerAvg = NULL

dums = cbind(dums, df['attrPartnerAvg'])
dums = interaction_maker(length(colnames(dums)), career_locs, dums)
colnames(dums)
dums$attrPartnerAvg = NULL

dums = dums[,colSums(dums) > 20]

dums = interaction_maker(race_locs, race_partner_locs, dums)
dums = interaction_maker(career_c_locs, career_c_partner, dums)

# c = (dums1[,1:6]*dums2[,1:6])
# count = 0
# for (i in cnames1){
#         idx = count*length(cnames1)+1
#         print(idx)
#         print(length(names(c)[idx:idx+length(cnames1)-1]))
#         names(c)[idx:idx+length(cnames1)-1] = lapply(cnames2, function(x) paste(i,x,sep=":"))
#         count = count + 1
# }

features = cbind(dums, df$decAvg,df$decPartnerAvg, df$attrPartnerAvg)
results = crossValidate(features, df$dec)

features_male = features[df$gender == "male",]
features_female = features[df$gender == "female",]
dim(features_male)
results_male = crossValidate(features_male, df$dec[df$gender == 'male'], waves = df$wave[df$gender == 'male'])

subset(df, df$gender == 1)
