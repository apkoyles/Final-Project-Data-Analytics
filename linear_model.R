#install libraries
library(ISLR)
library(data.table)
library(ggpubr)
library(corrplot)
library(psych)
library(MASS)
library(alr4)

#read in data
grad = fread(file = 'Data/final_graduate.csv')

#remove unnecessary column
grad = subset(grad, select = -V1)

#check distribution of continous variables (including response: Admit Chance)
# also check the skew and kurtosis of each to determine if transformation is necessary
ggplot(grad, aes(x = GRE)) + 
  geom_histogram(aes(y = ..density..), color = 'blue', fill = 'light blue', bins = 20) +
  geom_density(alpha = 0.05, color = 'red')

ggplot(grad, aes(x = TOEFL)) + 
  geom_histogram(aes(y = ..density..), color = 'blue', fill = 'light blue', bins = 20) +
  geom_density(alpha = 0.05, color = 'red')

ggplot(grad, aes(x = CGPA)) + 
  geom_histogram(aes(y = ..density..), color = 'blue', fill = 'light blue', bins = 20) +
  geom_density(alpha = 0.05, color = 'red')

describe(grad$GRE)
describe(grad$TOEFL)
describe(grad$CGPA)
describe(grad$Univ_Rating)
describe(grad$SOP)
describe(grad$LOR)
describe(grad$Admit_Chance)

## these graphs all indicate a fairly normal distribution, meaning no transformations are necessary
## the distributions and the associated skew and kurtosis calculations also indicate a pretty normal
## distribution

#create scatterplot matrix to check relationships
pairs(Admit_Chance~GRE+TOEFL+Univ_Rating+SOP+LOR+CGPA+Research, data = grad)
#correlation plot
corPlot(grad, cex = 0.7)

#use boxplots to further observe some relationships for the categorical regressors
boxplot(grad$Admit_Chance~grad$Research,
        xlab = 'Research Experience',
        ylab = 'Admission Chances',
        main = 'Boxplot of Admission for Research Experience')

boxplot(grad$Admit_Chance~grad$LOR,
        xlab = 'Letter of Recommendation',
        ylab = 'Admission Chances',
        main = 'Boxplot of Admission for Letter Rating')

boxplot(grad$Admit_Chance~grad$Univ_Rating,
        xlab = 'Rating of the University',
        ylab = 'Admission Chances',
        main = 'Boxplot of Admission for University Rating')

#clear  correlation between several of the variables and the response (Admit_chance)
# GRE, TOEFL, CGPA among the clearest relationships

#also need to check for multicollinearity because several variables appear to be multicollinear
# will do this after creating the model

#split data into training and test set(80/20)
set.seed(42)
splits = sort(sample(nrow(grad)*0.8))
train = grad[splits, ]
test = grad[-splits, ]

#build preliminary model
m_int = lm(Admit_Chance~1, data = train)
m_full = lm(Admit_Chance~GRE+TOEFL+Univ_Rating+SOP+LOR+CGPA+Research, data = train)
f = ~train$GRE+train$TOEFL+train$Univ_Rating+train$SOP+train$LOR+train$CGPA+train$Research

m_both = step(m_int, direction = 'both', scope = f)

#remove insignificant variables
m_final = lm(Admit_Chance~GRE+TOEFL+LOR+CGPA+Research, data = train)
summary(m_final)

#use vif to explore multicollinearity (scatterplots indicate the potential for severe multicollinearity)
vif(lm(Admit_Chance~GRE+TOEFL+LOR+CGPA+Research, data = train))
vif(lm(Admit_Chance~GRE+TOEFL, data = train))
vif(lm(Admit_Chance~GRE+TOEFL+CGPA, data = train))

## while a couple of the vifs (CGPA, GRE) are close to 5, none of these are high enough to warrant action
## all are less than 5

#test model with testing data set
preds = predict(m_final, newdata = test)

#plot predicted vs actual
ggplot(test, aes(x = preds, y = Admit_Chance)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  stat_cor(aes(label =..rr.label..), label.x = 0.5, label.y = 0.8) +
  ggtitle('Predicted vs. Actual') + 
  xlab('Predicted Chance of Admission') +
  ylab('Actual Chance of Admission')

#this model clearly does a pretty good job of predicting this data, with an r2 of 0.87

#next I will check the residuals for this model
plot(m_final)

#the residual plots show that they are not perfectly normal, indicating a change may be needed

#check the boxcox transformation to see if a transformation is needed on the response variable
bc = boxcox(m_final)
(lambda = bc$x[which.max(bc$y)])

#a lambda of 2 indicates that no transformation is necessary to improve this model
# from our previous analysis of the distribution of each predictor, we know that they all closely follow
# the normal distribution, indicating the problem with the residuals may be in outliers

#re run regression without outliers
#outliers removed were values 8, 45, and 68
no_outliers = grad[-c(8, 45, 68)]

#resplit data into test and train
set.seed(11)
splits2 = sort(sample(nrow(no_outliers)*0.8))
train2 = no_outliers[splits2, ]
test2 = no_outliers[-splits2, ]

#create regression model
m_noOutliers = lm(Admit_Chance~GRE+TOEFL+LOR+CGPA+Research, data = train2)
summary(m_noOutliers)

#test model with testing data set
preds2 = predict(m_noOutliers, newdata = test2)

#plot predicted vs actual
ggplot(test2, aes(x = preds2, y = Admit_Chance)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  stat_cor(aes(label =..rr.label..), label.x = 0.5, label.y = 0.8) +
  ggtitle('Predicted vs. Actual') + 
  xlab('Predicted Chance of Admission') +
  ylab('Actual Chance of Admission')

#Model still predicts well (0.79 r^2 for model, 0.83 on predictions)

#check residual plots
plot(m_noOutliers)

#based on this, we still have more outliers and the residuals are not any more normally distributed
# although this is not as desirable as perfectly normal residuals, it is close enough that it should not
# be a large problem
# These outliers are also a natural part of the population (not data entry errors) so it is not as big of
# a deal that they are there
# the model also does a good job so I will move forward with the model that includes those outliers

#final model
m_final = lm(Admit_Chance~GRE+TOEFL+LOR+CGPA+Research, data = train)
summary(m_final)

#calculate test MSE for this model
MSE = mean((test$Admit_Chance - predict.lm(m_final, test))^2)
MSE

#a MSE of 0.004 (less than a percent) is highly encouraging and indicates that this model does a good
# job of predicting admission chances given the predictors



