library('HistData')
library('Rmisc')
library('dplyr')

data("GaltonFamilies")
df = GaltonFamilies
View(df)

names(df)
df$gender = as.numeric(df$gender) -1 

gf = df %>%
  group_by(family) %>%
  dplyr::summarise(avg = mean(childHeight), mid = mean(midparentHeight),
                   father = mean(father), mother = mean(mother), count = n(), num_m = sum(gender)) %>%
  dplyr::mutate(pmale = num_m / count)
#male is 1, female is 0

gf = na.omit(gf)

scatterplot(gf$mid, gf$avg)
?scatterplot

?multiplot
?ggplot

p1= ggplot(gf, aes(x=avg, y=mother))+ geom_line()
p2= ggplot(gf, aes(x=avg, y=father))+ geom_line()
p3= ggplot(gf, aes(x=avg, y=mid))+ geom_line()
multiplot(p1,p2,p3)

cor(gf$avg, gf$mid)
scatterplot(gf$avg, gf$mid)
scatterplot(gf$avg, gf$father)
scatterplot(gf$avg, gf$mother)

regressFather = lm(avg~father, gf)
regressMother = lm(avg~mother, gf)
regressMid = lm(avg~mid, gf)
summary(regressMid)[9]
summary(regressMother)[9]
summary(regressFather)[9]

regressFather = lm(count~father, gf)
regressMother = lm(count~mother, gf)
regressMid = lm(count~mid, gf)
regressAll = lm(count~mother+father, gf)
summary(regressAll)
summary(regressMid)
summary(regressMother)
summary(regressFather)

regressFather2 = lm(avg~father, gf)
regressMother2 = lm(avg~mother, gf)
regressMid2 = lm(avg~mid, gf)
regressGender2=lm(avg~pmale,gf)
summary(regressGender2)
summary(regressMid2)
summary(regressMother2)
summary(regressFather2)

regressFather = lm(count~father, gf)
regressMother = lm(count~mother, gf)
regressMid = lm(count~mid, gf)
regressAll = lm(count~mother+father, gf)
summary(regressAll)
summary(regressMid)
summary(regressMother)
summary(regressFather)

ungroup(gf)

maleDF = filter(df, gender==1)
regressMale = lm(childHeight~father, maleDF)
summary(regressMale)

regressFemale = lm(childHeight~mother, maleDF)
summary(regressFemale)


