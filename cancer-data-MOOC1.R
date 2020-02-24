library(tidyverse)

# load data
g <- read_csv("cancer data for MOOC 1.csv")

# summary of variables

# 1- age
summary(g$age)
hist(g$age)

# 2- gender
gender <- as.factor(g$gender)
summary(gender)

# 3- BMI
summary(g$bmi)
hist(g$bmi)

# 4- smoking
smoking <- as.factor(g$smoking)
summary(smoking)

# 5- exercise
exercise <- as.factor(g$exercise)
summary(exercise)

# 6- fruit
summary(g$fruit)
hist(g$fruit)
fruit <- as.factor(g$fruit)
summary(fruit)

# 7- veg
summary(g$veg)
hist(g$veg)
veg <- as.factor(g$veg)
summary(veg)

# 8- cancer
summary(g$cancer)
hist(g$cancer)
cancer <- as.factor(g$cancer)
summary(cancer)

# fruit + veg
g$fruitveg <- g$fruit + g$veg
summary(g$fruitveg)

# five a day
g$five_a_day <- ifelse(g$fruitveg >= 5 , 1, 0)
table(g$five_a_day)

# Plots

# fruitveg
hist(g$fruitveg)

hist(g$fruitveg, xlab = "Portions of fruit and vegetables",
     main = "Daily consumption of fruit and vegetables", axes = F)
axis(side = 1, at = seq(0, 11, 1))
axis(side = 2, at = seq(0, 16, 2))

#ggplot fruitveg
ggplot()+ geom_histogram(data = g, aes(x= fruitveg),
                         bins = 10, fill = "darkgreen", col= "black")+
  labs(x = "Protions of fruit and vegetables", y = "Frequency")+
  scale_x_continuous(breaks = seq(from = 0, to = 12 , by = 1))+
  theme_bw()

# healthy BMI
bmi <- ifelse(g$bmi <=18.4|g$bmi >=25, "not normal", "normal")
table(bmi)

g$healthy_bmi <- ifelse(g$bmi > 18.5 & g$bmi < 25, 1, 0)
table(g$healthy_bmi)

# Plots

# fruit
hist(g$fruit, xlab = "Portions of fruit",
     main = "Daily consumption of fruit", axes = F)
axis(side = 1, at = seq(0, 4, 1))
axis(side = 2, at = seq(0, 24, 4))

# veg
hist(g$veg, xlab = "Portions of vegetables",
     main = "Daily consumption of vegetables", axes = F)
axis(side = 1, at = seq(0, 9, 1))
axis(side = 2, at = seq(0, 18, 2))

# ggplot fruit
ggplot()+ geom_histogram(data = g, aes(x = fruit), bins = 5,
                         fill = "darkgreen", col = "black")+
  theme_bw()+ labs(x = "Portions of fruit", y = "Frequency")+
  
  scale_x_continuous(breaks = seq(from = 0, to = 4, by = 1))

# ggplot veg
ggplot() + geom_histogram(data = g, aes(x = veg), bins = 10,
                          fill = "darkgreen", col = "black")+
  theme_bw()+ labs(x = "Portions of vegetables", y = "Frequency")+
  scale_x_continuous(breaks = seq(from = 0, to = 9, by = 1))

# Running a Chi-square test

chisq.test(x = g$five_a_day, y = cancer)

# Running an independent sample t-test

t.test(g$bmi ~ cancer)

# If you want to test whether 
# the mean of y equals some value of interest:

t.test(g$bmi,mu=25) # the null hypothesis here is that the mean BMI is 25

# dichotomise BMI into a yes/no (1/0) variable with the threshold of 25
overweight <- ifelse(g$bmi > 25, 1, 0)
table(overweight)

chisq.test(x = overweight, y = cancer)
