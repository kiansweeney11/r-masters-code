## Kian Sweeney
## MBY1
## 22220670
## MS5108 Assignment 3
## Due Date: 6/4/23
library(ggplot2)
library(readr)
library(plotly)
library(dplyr)
library(lubridate)
library(xts)
library(stringr)

survey = read_csv("Crossfit-Ridgeline-DataSet.csv")
survey

### check number of nulls per row
rowSums(is.na(survey))

### only ten rows with null values
## most of these contain a number of null columns
## we will remove all these due to the small number of null rows

survey_rm <- na.omit(survey)

sapply(survey_rm, class)

### check means for each column that is numeric
colMeans(survey_rm[sapply(survey_rm, is.numeric)])

## check spread of gender values
table(survey_rm['Gender'])

box_df = survey_rm

box_df %>% mutate(Gender = recode(Gender, 'Male' = 1, 'Female' = 2))

box_df$Gender = as.character(box_df$Gender)
sapply(box_df, class)

box_df %>% mutate(Gender = recode(Gender, `1` = 'Male', `2` = 'Female'))

survey_rm <- survey_rm %>% rename("MembershipTenure" = "Membership Tenure")

boxplot(MembershipTenure~Gender,
        data = survey_rm,
        main = "Boxplots of membership length in months",
        xlab = "Gender",
        ylab = "Term in Months",
        col = "dodgerblue",
        border = "black"
)

boxplot(CUSSAT1~Gender,
        data = survey_rm,
        main = "Boxplots of CUSSAT1 per Gender",
        xlab = "Gender",
        ylab = "CUSSAT1",
        col = "deepskyblue",
        border = "black"
)

age_vals <- survey_rm$Age
empty_list <- list()
for(x in age_vals){
  if (x >= 18 & x <= 25){
    current_res = "18-25"
    empty_list <- append(empty_list, current_res) 
  }
  else if (x > 25 & x <= 35){
    current_res = "26-35"
    empty_list <- append(empty_list, current_res) 
  }
  else if (x > 35 & x <= 45){
    current_res = "36-45"
    empty_list <- append(empty_list, current_res) 
  }
  else {
    current_res = "45+"
    empty_list <- append(empty_list, current_res) 
  }
}

#empty_list
survey_rm$AgeRange = empty_list

survey_rm$AgeRange = as.character(survey_rm$AgeRange)

#survey_rm %>% mutate(AgeRange = recode(AgeRange, `18-25` = "1", `26-35` = "2", `36-45` = "3", `45+` = "4" ))

### PLOT FOR ANSWER IGNORE PREVIOUS BOXPLOTS!

boxplot(CUSSAT1~AgeRange,
        data = survey_rm,
        main = "Boxplot of CUSSAT1 per Age Range",
        xlab = "Age Range",
        ylab = "Overall Membership Happiness",
        col = "deepskyblue",
        border = "black"
)

mean_cus1 <- survey_rm %>% group_by(AgeRange) %>% 
  summarise(mean_cussat1=mean(CUSSAT1))
mean_cus1

### plot 2
### grouped bar chart showing mean CUSSAT4 per gender/age group
install.packages("lattice")
library("lattice")

gender_vals <- survey_rm$Gender
empty_list_gen <- list()
for(x in gender_vals){
  if (x == 1){
    current_res1 = "Male"
    empty_list_gen <- append(empty_list_gen, current_res1) 
  }
  else {
    current_res1 = "Female"
    empty_list_gen <- append(empty_list_gen, current_res1) 
  }
}

## add string column to df
survey_rm$GenderString = empty_list_gen

survey_rm$GenderString = as.character(survey_rm$GenderString)

mean_plots <- survey_rm %>% group_by(GenderString, AgeRange) %>% 
  summarise(mean_cussat4 = mean(CUSSAT4))
mean_plots

## set diff colours for bars
colors_bar_grouped = c("cornflowerblue", "goldenrod2")

my.settings <- list(
  superpose.polygon = list(col = colors_bar_grouped)
)

barchart(AgeRange ~ mean_cussat4,
         main = "Group Bar Plot of Mean CUSSAT4 Per Gender and Age Group",
         data = mean_plots,
         ylab = "Age Group",
         xlab = "Mean Coach Responsibilties Performed Rating",
         groups = GenderString, par.settings = my.settings,
         auto.key = TRUE)

table(survey_rm['AgeRange'])

##### Q3
## regression 1
#install.packages("report")
library("report")

# check correlations
df_numeric = subset(survey_rm, select = -c(AgeRange,GenderString))
df_numeric
correlations <- round(cor(df_numeric), 2)
head(correlations)

library(reshape2)
## create correlation matrix
correlation_mat <- melt(correlations)
head(correlation_mat)

## plot correlation matrix
## try find more correlated variables for beter LR model
ggplot(data = correlation_mat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "orangered3", high = "seagreen4", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1))+
  coord_fixed()

### model building
model1 <- lm(COASAT2 ~ COASAT3 + COASAT5 + CLIM2, data = survey_rm)
summary(model1)
report(model1)

confint(model1)
plot(model1)

## RSE
sigma(model1)/mean(survey_rm$COASAT2)

#### MODEL 2

# center the variables (IGNORE)
#cussat2_c <- survey_rm$CUSSAT2 - mean(survey_rm$CUSSAT2)
#gender_c <- survey_rm$Gender - mean(survey_rm$Gender)

# create interaction variable (IGNORE)
#survey_rm$gender_x_cussat2 <- gender_c * cussat2_c

## lets firstly plot CUSSAT1 VS CUSSAT2 by gender
ggplot(survey_rm, aes(x = CUSSAT1, y = CUSSAT2, col = GenderString)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE)
## slightly different regression lines lets delve deeper

######
# dependent - CUSSAT1
# independent - CUSSAT2
# interaction - Gender
######
## above methods focus on centering variables for correlation reasons but can be ignored here
## followed this method
## above comments look at another method i found for interaction vars (IGNORE)
# https://quantifyinghealth.com/linear-regression-with-interaction-in-r/
interactionModel <- lm(CUSSAT1 ~ CUSSAT2 + Gender + CUSSAT2:Gender, data = survey_rm)
summary(interactionModel)
report(interactionModel)

## check r-squared impact - interaction vs no interaction
# no interaction
summary(lm(CUSSAT1 ~ CUSSAT2 + Gender, data = survey_rm))$r.squared
# interaction
summary(lm(CUSSAT1 ~ CUSSAT2 + Gender + CUSSAT2:Gender, data = survey_rm))$r.squared
# difference found

## check p-value diff
summary(lm(CUSSAT1 ~ CUSSAT2 + Gender + CUSSAT2:Gender, data = survey_rm))$coefficients[,4]
# no interaction
summary(lm(CUSSAT1 ~ CUSSAT2 + Gender, data = survey_rm))$coefficients[,4]
## intercept now not significant with interaction

#install.packages("interactions")
library(interactions)
## check if gender lines intersect
interact_plot(interactionModel, pred = CUSSAT2, modx = Gender, plot.points = TRUE)
# run anova - explain effect of gender and cussat2 on cussat1
## focusing on CUSSAT2:Gender output here
## if below 0.05 - there is significant interaction effect between cussat2 and gender
anova <- aov(CUSSAT1 ~ CUSSAT2 * Gender, data = survey_rm)
summary(anova)

