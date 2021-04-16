# import Excel file from drive
library(readxl)
newdata <- read_excel("student_data.xlsx")
head(newdata)

# Some packages
library(e1071) # it includes function to compute skewness
library(plyr) # it allows wrangling data
library(ggplot2) # it allows creating a number of different types of plots

# Graphical analysis
# displays data into quartiles and allows to detect any outliers
par(mfrow=c(1, 2))  # it divides graph area in two parts
boxplot(newdata$AvgScore, col = "yellow", border="blue",
        main = "Average Score boxplot",
        ylab = "Score (%)")
boxplot(newdata$Time, col = "orange", border="blue",
        main = "Time data boxplot",
        ylab = "Time (min)")

# outliers
boxplot.stats(newdata$AvgScore)$out
boxplot.stats(newdata$Time)$out

# Scatter Plot
qplot(Time, AvgScore, data = newdata,
      main = "Time on Canvas and Average Score relationship") +
  geom_point(colour = "blue", size = 1.5)

#Histogram to check the normality of data distribution

# Histogram of Average Score
qplot(AvgScore, data = newdata, geom="histogram", binwidth=10) +
  labs(title = "Average Score") +
  labs(x ="Average Score (%)") +
  labs(y = "Frequency") +
  geom_vline(xintercept = mean(newdata$AvgScore),
             show_guide=TRUE, color="red", labels="Average") +
  geom_vline(xintercept = median(newdata$AvgScore),
             show_guide=TRUE, color="blue", labels="Median")

# Histogram of Time
qplot(Time,data = newdata, geom="histogram", binwidth=200) +
  labs(title = "Time Data") +
  labs(x ="Time (min)") +
  labs(y = "Frequency") +
  geom_vline(xintercept = mean(newdata$Time),
             show_guide=TRUE, color="red", labels="Average") +
  geom_vline(xintercept = median(newdata$Time),
             show_guide=TRUE, color="blue", labels="Median")

# Linear Model building
qplot(Time, AvgScore, data = newdata,
      main = "Time on Canvas and Average Score relationship") +
  stat_smooth(method="lm", col="red", size=1) +
  geom_point(colour = "blue", size = 1.5)

#Linear Model Analysis
mod<- lm(AvgScore ~ Time, data=newdata)
summary(mod)


# t-statistics test
modSummary <- summary(mod)  # capture model summary as an object
modCoeff <- modSummary$coefficients  # model coefficients
beta.estimate <- modCoeff["Time", "Estimate"]  # get beta coefficient estimate
std.error <- modCoeff["Time", "Std. Error"]  # get standard error
t_value <- beta.estimate/std.error  # calculate t statistic
print(t_value) # print t-value

# Validation of Regression Analysis

# 1. Residuals vs Fitted values
par(mfrow = c(1,1))
plot(mod, pch=16, col="blue", lty=1, lwd=2, which=1)

# 2. Normal Q-Q
par(mfrow = c(1,1))
plot(mod, pch=16, col="blue", lty=1, lwd=2, which=2)

# 3. summary of all plot
par(mfrow = c(2,2)) # display a unique layout for all graphs
plot(mod)
