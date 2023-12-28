# Joanna's part on steps 3 & 4

install.packages("psych")
install.packages("car")
install.packages("lmtest")
install.packages("plm")

IMDB_data = read.csv("C:/Users/Joanna Koo/OneDrive - McGill University/McGill University/Analytics/MGSC 661/Midterm project/IMDB_data_Fall_2023_clean.csv")
attach(IMDB_data)

## 1. Examine correlations between all predictors
# Correlation matrix of all predictors
require(psych)
options(max.print = 100000)

quantvars_all = IMDB_data[,]
corr_matrix_all = cor(quantvars_all)
round(corr_matrix_all, 2)

# Correlation matrix for select predictors
quantvars_select = IMDB_data[, c(1, 2, 3, 4, 5, 6, 7, 8, 23, 24)]
corr_matrix_select = cor(quantvars_select)
round(corr_matrix_select, 2)

## 2. Look at scatter plot between Y and each xi, and run ncv test
# movie_budget
plot(movie_budget, imdb_score)

library(car)
reg1 = lm(imdb_score~movie_budget)
abline(reg1)
qqPlot(reg1, envelope=list(style="none"))
ncvTest(reg1)

outlierTest(reg1)

# release_year
plot(release_year, imdb_score)

reg2 = lm(imdb_score~release_year)
abline(reg2)
qqPlot(reg2, envelope=list(style="none"))
ncvTest(reg2)

outlierTest(reg2)

# duration
plot(duration, imdb_score)

reg3 = lm(imdb_score~duration)
abline(reg3)
qqPlot(reg3, envelope=list(style="none"))
ncvTest(reg3)

outlierTest(reg3)

# nb_news_articles
plot(nb_news_articles, imdb_score)

reg4 = lm(imdb_score~nb_news_articles)
abline(reg4)
qqPlot(reg4, envelope=list(style="none"))
ncvTest(reg4)

outlierTest(reg4)

# actor1_star_meter
plot(actor1_star_meter, imdb_score)

reg5 = lm(imdb_score~actor1_star_meter)
abline(reg5)
qqPlot(reg5, envelope=list(style="none"))
ncvTest(reg5)

outlierTest(reg5)

# actor2_star_meter
plot(actor2_star_meter, imdb_score)

reg6 = lm(imdb_score~actor2_star_meter)
abline(reg6)
qqPlot(reg6, envelope=list(style="none"))
ncvTest(reg6)

outlierTest(reg6)

# actor3_star_meter
plot(actor3_star_meter, imdb_score)

reg7 = lm(imdb_score~actor3_star_meter)
abline(reg7)
qqPlot(reg7, envelope=list(style="none"))
ncvTest(reg7)

outlierTest(reg7)

# movie_meter_IMDBpro
plot(movie_meter_IMDBpro, imdb_score)

reg8 = lm(imdb_score~movie_meter_IMDBpro)
abline(reg8)
qqPlot(reg8, envelope=list(style="none"))
ncvTest(reg8)

outlierTest(reg8)

# aspect_ratio_2.35
plot(aspect_ratio_2.35, imdb_score)

reg9 = lm(imdb_score~aspect_ratio_2.35)
abline(reg9)
qqPlot(reg9, envelope=list(style="none"))
ncvTest(reg9)

outlierTest(reg9)

## 3. Remove outliers (Rows 183, 303, 377, 468, 935, 1487, 1694)
IMDB_data2 = IMDB_data[-c(183, 303, 377, 468, 935, 1487, 1694), ]

detach(IMDB_data)
attach(IMDB_data2)

# Fix heteroskedasticity
require(lmtest)
require(plm)

coeftest(reg1, vcov=vcovHC(reg1, type='HC1'))
qqPlot(reg1, envelope=list(style="none"))
