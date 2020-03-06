library(dplyr)
library(readr)
data <- readr::read_csv("phpTJRsqa.csv")
install.packages("tableone")
install.packages("dlookr")

"""
Attribute Information:

For more information, read [Cortez et al., 2009].
Input variables (based on physicochemical tests):
1 - fixed acidity
2 - volatile acidity
3 - citric acid
4 - residual sugar
5 - chlorides
6 - free sulfur dioxide
7 - total sulfur dioxide
8 - density
9 - pH
10 - sulphates
11 - alcohol
Output variable (based on sensory data):
12 - quality (score between 0 and 10)
"""

data$Class <- as.factor(data$Class)
library(dlookr)
eda_report(data, target = Class)

features <- data[, -12]
dimnames(features)
corrplot::corrplot(features)

for(i in colnames(features)){
  print(max(features[i]) - min(features[i]))
}


install.packages("mlr3")
library(mlr3)

relation <- target_by(data, Class)
relo <- relate(relation, V4)
plot(relo)

##------------------
# WNIOSKI
# 1. Znormalizować zakresy zmiennych
# 2. Zależności - (V6, V7), (V4, V8)
# 3. Małe liczności klas njalepszych, najgorszych
# 4.  
