#install libraries
library(ggplot2)
library(data.table)
library(dplyr)
library(psych)

#read in data
grad = fread(file = 'Data/Admission_Predict.csv')

#rename columns for easier use
grad = grad %>% rename('GRE' = 'GRE Score', 'TOEFL' = 'TOEFL Score', 'Univ_Rating' = 'University Rating',
                       'Admit_Chance' = 'Chance of Admit')

#remove NAs (none in this dataset)
grad = na.omit(grad)

#review the distribution of all variables
summary(grad)

#check distribution of response variable: admit_chance
## histogram
ggplot(grad, aes(x = Admit_Chance)) + 
  geom_histogram(aes(y = ..density..), color = 'blue', fill = 'light blue', bins = 20) +
  geom_density(alpha = 0.05, color = 'red')

describe(grad$Admit_Chance)

## not a perfectly normal distribution, but it is close enough to justify no transformations

## boxplot with listed outliers

boxplot(grad$Admit_Chance,
        ylab = "Chance of Admission",
        main = "Boxplot of Admission Chances"
)
out = boxplot.stats(grad$Admit_Chance)$out
mtext(paste("Outliers: ", paste(out, collapse = ", ")))

## Despite the presence of two outliers, we will not remove them because they are a natural part of the 
  ## population

ggplot(grad, aes(x = GRE)) + 
  geom_histogram(aes(y = ..density..), color = 'blue', fill = 'blue', bins = 20) +
  geom_density(alpha = 0.05, color = 'red')

ggplot(grad, aes(x = TOEFL)) + 
  geom_histogram(aes(y = ..density..), color = 'blue', fill = 'blue', bins = 20) +
  geom_density(alpha = 0.05, color = 'red')

ggplot(grad, aes(x = LOR)) + 
  geom_histogram(aes(y = ..density..), color = 'blue', fill = 'blue', bins = 20) +
  geom_density(alpha = 0.05, color = 'red')

ggplot(grad, aes(x = CGPA)) + 
  geom_histogram(aes(y = ..density..), color = 'blue', fill = 'blue', bins = 20) +
  geom_density(alpha = 0.05, color = 'red')

boxplot(grad$Admit_Chance~grad$Research,
        xlab = 'Research Experience',
        ylab = 'Admission Chances',
        main = 'Boxplot of Admission for Research Experience')

#write final dataset as a csv
write.csv(grad, 'Data/final_graduate.csv')
