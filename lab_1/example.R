measles = readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv')
summary(measles)
#Get first few observations
head(measles)
#Get last few observations
tail(measles)
#get total number of columns in dataset
ncol(measles)
#get total number of rows/observations
nrow(measles)
#get total number of observations for measles and mumps vaccination rate [A PARTICULAR COLUMN]
length(measles$mmr)
#PLOT FIRST 100 OBSERVATIONS OF THE Percentage of students exempted from vaccination for personal reasons
plot(measles$xper[1:100],
     main = "Sample percentage plot of students exempted from vaccination for personal reasons",
     ylab = "% of students vaccinated",
     xlab = "Percentage (%)")

# Checking for Missing values
#Explore number of vaccination exemptions from the 220th to the 430th observation
measles$xmed[220:430]

#we notice there are NAs
na.omit(measles$xmed[220:430])

#we notice there are now no NAs
#let's count the number of vaccination exemptions from the 220th to the 430th observation WITH NO NAs
length(na.omit(measles$xmed[220:430]))

#PLOT SAMPLE OBSERVATIONS OF THE Percentage of students exempted from vaccination for personal reasons
plot(na.omit(measles$xmed[220:430]),
     main = "Sample percentage plot of students exempted from vaccination for personal reasons",
     ylab = "% of students vaccinated",
     xlab = "Percentage (%)")

#Select all observations of students exempted for medical reasons from Arizona
measles$xmed[measles$state == "Arizona"]

#show less sparse observations from the previous statement
na.omit(measles$xmed[measles$state == "Arizona"])

plot(measles$xmed[measles$state == "Arizona"])

plot(na.omit(measles$xmed[measles$state == "Arizona"]))

library(dplyr)

unique(measles$state)

library(scatterplot3d)

#select only 100 observations of medical exemption reason data for 3 states
ar <- na.omit(measles$xmed[measles$state == "Arizona"])[1:100]
ca <- na.omit(measles$xmed[measles$state == "California"])[1:100]
wa <- na.omit(measles$xmed[measles$state == "Washington"])[1:100]

#we selected 100 so that plotting will be easier
scatterplot3d(ar, ca, wa, pch = 16, highlight.3d = TRUE,
              type = "h", main = "3D Scatterplot of Vaccination Exemptions due to Medical Reasons",
              xlab = "Arizona %", ylab = "California %", zlab = "Washington %")

boxplot(ar, ca, wa, names = c("Arizona", "California", "Washington"))

set.seed(1)

library(mice)
library(VIM)

mice_plot <- aggr(measles, col = c('green', 'red'),
                  numbers = TRUE, sortVars = TRUE,
                  labels = names(measles), cex.axis = .7,
                  gap = 3, ylab = c("Missing data", "Pattern"))

names(measles)

measles_clean = measles[c(-8, -12)]

names(measles_clean)

imputed_Data <- mice(measles_clean, m = 1, maxit = 1, method = 'pmm', seed = 500)

completeData <- complete(imputed_Data, 1)

write.csv(file = "complete_measlesDF.csv", completeData, fileEncoding = "utf-8")
