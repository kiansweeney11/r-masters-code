#### Kian Sweeney, MBY1, 22220670
#### MS5108 Assignment 1
#### Due Date: 9/2/23

library(ggplot2)

## q1 part 1

x1 <- rnorm(50)
x2 <- rexp(50)

### linear combo (following this method https://wims.univ-cotedazur.fr/wims/wims.cgi)
y <- x1 + x2

## make df
vector_df <- data.frame(x1, x2, y)
vector_df

### keep copy of df used in this experiment
#write.csv(vector_df, "r-data/dataframe-kiansweeney-ms5018-assignment1.csv", row.names=FALSE)

#### read in actual df used
#file <- "r-data/dataframe-kiansweeney-ms5018-assignment1.csv"
#vector_df = read_csv(paste0(file))

## multiple LR
linear_model <- lm(y ~ x1 + x2, data = vector_df)

# Rounded for easier viewing
lm.betas <- round(linear_model$coefficients, 2)
lm.betas

# Create dataframe of results
results <- data.frame(lm.results=lm.betas)

print(results)

summary(linear_model)
plot(linear_model)

### confidence interval of model
confint(linear_model)

##### Q2
## convert to df
Subject <- c(1:10)

Height <- c(1.82, 1.56, 1.74, 1.55, 1.63, 1.91,
            2.05, 1.84, 1.80, 1.71)

Weight <- c(80.4, 66.2, 68.9, 70.1, 75, 83.7,
            105.6, 79.5, 68, 69.4)

bmi_df <- data.frame(Subject, Height, Weight)
bmi_df

## bmi calculator
bmi_first <- bmi_df$Weight / bmi_df$Height
BMI <- bmi_first / bmi_df$Height

bmi_df <- data.frame(Subject, Height, Weight, BMI)
bmi_df

### part 2

#### SD of columns
sapply(bmi_df[c('Height', 'Weight', 'BMI')], sd)

#### Mean of columns
sapply(bmi_df[c('Height', 'Weight', 'BMI')], mean)

### Part 3
sample <- bmi_df[bmi_df$Height >= 1.70 & bmi_df$Weight < 70,]
sample

### Part 4

#### SD of sample
sapply(sample[c('Height', 'Weight', 'BMI')], sd)

#### Mean of sample
sapply(sample[c('Height', 'Weight', 'BMI')], mean)

## pair plots for full sample
## check correlations between all variables of interest
pairs(bmi_df[c('Height', 'Weight', 'BMI')])

#### Part 5 Comparisons

#### not including height as it isnt on same scale (much smaller values than other cols)
dfcomp <- rbind(sapply(bmi_df[c('Weight', 'BMI')], mean), sapply(sample[c( 'Weight', 'BMI')], mean))

barplot(dfcomp, main="Mean Comparison", col=c("darkblue","red"), beside=TRUE)

### now sd
dfcomp_sd <- rbind(sapply(bmi_df[c('Weight', 'BMI')], sd), sapply(sample[c('Weight', 'BMI')], sd))
dfcomp_sd

barplot(dfcomp_sd, main="SD Comparison", col=c("darkblue","red"),
        beside=TRUE, legend = TRUE)

## pair plots for sample
## check correlations between all variables of interest
# note the almost perfect correlations (straight line through 0 between variables here)
pairs(sample[c('Height', 'Weight', 'BMI')])

### significance tests
### bmi mean was 24.79132 for full population
### lets check if this significantly difference for our pop
t_bmi <- t.test(sample$BMI, mu = 24.8)
# Printing the results
t_bmi
t_bmi$p.value
### key takeways from this result
# the difference in mean bmi is not statistically
# significant between our sample and full population
# p-value = 0.103 which is greater than sig. level of 0.05

## t-test of weight
# mean weight of all 10: 76.68000
t_weight <- t.test(sample$Weight, mu = 76.68000)
# Printing the results
t_weight
t_weight$p.value

# mean height of all 10 - 1.76100: 
t_height <- t.test(sample$Height, mu = 1.76100)
# Printing the results
t_height
t_height$p.value
### reject null hypothesis , means not statistically significant 
## in difference for height in overall pop and sample