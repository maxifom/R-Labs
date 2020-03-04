library(mice)
library(VIM)
library(scatterplot3d)
library(dplyr)


measles = readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv')
students_count = nrow(measles)


measles$xrel = as.double(measles$xrel)
measles$xrel = replace(measles$xrel, is.na(measles$xrel), 0)
measles$xrel = replace(measles$xrel, measles$xrel == 1, 100)
personal_reasons = measles %>% summarise(average = mean(na.omit(measles$xper)))
personal_reasons = personal_reasons$average
medical_reasons = measles %>% summarise(average = mean(na.omit(measles$xmed)))
medical_reasons = medical_reasons$average
religious_reasons = measles %>% summarise(average = mean(na.omit(measles$xrel)))
religious_reasons = religious_reasons$average


ny <- na.omit(measles$xmed[measles$state == "New York"])[1:50]
or <- na.omit(measles$xmed[measles$state == "Oregon"])[1:50]
cc <- na.omit(measles$xmed[measles$state == "Connecticut"])[1:50]
scatterplot3d(ny, or, cc, pch = 16, highlight.3d = TRUE,
              type = "h",
              main = "3D",
              xlab = "New york %",
              ylab = "Oregon %",
              zlab = "Connecticut %")

measles$overall = replace(measles$overall, measles$overall == -1, 0)
barplot(measles$overall[measles$state == "California"], ylab = "Vaccinations rate", col = "blue",
        main = "Vaccinations rate in CA", border = "green")