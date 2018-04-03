data_new <- read.csv("C://Users//tanuj//Desktop//Subjects//SDM//Data Sets//Rossmann_data_new.csv")
head(data_new)
hist(data_new$Sales)
data1 <- data_new[ , data_new$Date == "2015"]
head(data1)
hist(log(data_new$Sales))


model_sales <- lm(Sales~CompetitionDistance,data = data_new)
summary(model_sales)
m2 <- lm(log(Sales)~Customers,data = data_new)

summary(m2)

plot(model_sales1$residuals~model_sales$fitted.values)

hist(m2$residuals)
hist(m2$fitted.values)

plot(m2$residuals,m2$fitted.values)
qqnorm(m2$residuals)
qqline(m2$residuals,col="red")
norm1 <- rnorm(50)
ks.test(norm1,m2$residuals)



d <-read.csv("C://Users//tanuj//Desktop//Subjects//SDM//Data Sets//Rossmann_data_2013.csv")
head(d)
m2 <- lm(log(Sales)~log(Customers),data = d)
summary(m2)

plot(m2$residuals,m2$fitted.values)
qqnorm(m2$residuals)
qqline(m2$residuals,col="red")
norm1 <- rnorm(50)
ks.test(norm1,m2$residuals)

m3 <-  lm(Sales~CompetitionDistance+Customers+Customers*CompetitionDistance,data = data_new)
summary(m3)

cov(d$Sales,d$CompetitionDistance)
cov(d$Sales,d$Customers)
cor(d$Sales,d$Customers)
cor(d$Sales,d$CompetitionDistance)
cor(d$Sales,d$Promo)
cor(d$Sales,d$Promo2)
set.seed(134000)
d2 <- sample_n(d,700,na.action())

m4 <-lm(log(Sales)~Customers,na.action=na.omit,data = d2)
summary(m4)

#Hypothesesis 1
#As the distance between competiter store increases the sales increaases.
#2.If the promotion is applied for one day sales increase as compared to 2nd day.


#Assortments :: a cat - day to day products like johnson and johnson,nestle(drug store+) #b- food wines perfumes, c- toys and stationary

m1 <- lm(Sales~log(CompetitionDistance)+Customers,data = data_new)
summary(m1)
hist(log(data_new$CompetitionDistance))
hist(m1$residuals)
hist(m1$fitted.values)
plot(m1$residuals,m1$fitted.values)
qqnorm(m1$residuals)
qqline(m1$residuals,col="red")
norm1 <- rnorm(100)
ks.test(norm1,m1$residuals) # data is not normal

vif(m1)  #no multicolinearity found

bartlett.test(list(m1$residuals,m1$fitted.values)) #not homoskedastic
plot(m1$residuals,m1$fitted.values)
plot(m1$residuals,data_new$Sales)
dwtest(m1)


summary(m1)
hist(m1$residuals)
hist(m1$fitted.values)
plot(m1$residuals,m1$fitted.values)
hist(data_new$Sales)
hist(log(data_new$Sales))
bartlett.test(list(m1$residuals,m1$fitted.values))

#####################PANNEL DATA ANALYSIS##############################
d <- read.csv("C://Users//tanuj//Desktop//Subjects//SDM//Project//Rossmann_data_new.csv")
head(d)

#POOLED OLS ESTIMATE
attach(d)
pooled <- plm(Sales~Promo+CompetitionDistance,data = d,index = "Assortment",model = "pooling")
summary(pooled)

#FIXED EFFECT MODEL
fixed <- plm(Sales~ as.factor(Promo)+CompetitionDistance,data = d,index = c("Assortment"),model = "within")
summary(fixed)
class(Assortment)
fixef(fixed)
pFtest(fixed,pooled) # fixed effect model is better
class(Promo)
#RANDOM EFFEXT MODEL
random <- plm(Sales~ Promo+CompetitionDistance,data = d,index = c("YEAR","Assortment"),model = "random")
summary(random)

pdim(d)
