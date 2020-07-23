# CS544 Final Project

install.packages("tidyverse")
library(tidyverse)

# Preparing the data
dataset <- read.csv("~/Downloads/StudentsPerformance.csv") 
dataset
view(dataset)

# Take a look at the variable names and types
glimpse(dataset)

# See for missing values in the dataset
is.na(dataset)

# Analyzing the data
# Analyze at least one categorical variable
x <-table(dataset$race.ethnicity)
df <- data.frame(x)
df

# Total number of unique categories
nrow(df) 

# Graphical representations of categorical Data
barplot(table(dataset$race.ethnicity),col = "cyan", ylim=c(0,350), xlab = "Group Type", ylab = "Frequency", 
        main = "Barplot of Main Group for Race")

# A bartplot showing the frequencies of race.ethnicity
barplot(table(dataset$race.ethnicity), horiz = TRUE,
        col = "cyan", xlim=c(0,350),las=2, xlab = "Frequency",  
        main = "Barplot of Main Group for Race")

# A pie chart of the race.ethnicity for comparing the propotions of each of the categories
pie(table(dataset$race.ethnicity))

data <- table(dataset$race.ethnicity)
slice.labels <- names(data)
slice.percents <- round(data/sum(data)*100)
slice.labels <- paste(slice.labels, slice.percents)
slice.labels <- paste(slice.labels, "%", sep="")
slice.labels
pie(data, labels = slice.labels, col=hcl(c(0, 60, 120)))

# Analyze at least one numerical variable

# Show the math score in the exam
x <- c(dataset$math.score)
x

# The mean of math socre variable 
mean(x)

# The median of math score variable
median(x)

# The range of math score variable
range(x)
diff(range(x))

# The variance and the standard deviation of the math score variable
var(x)
sd(x)

# Five-number summary of the math socre data
fivenum(x)
summary(x)

# Calculate explicitly the appropriate precentiles
quantile(x, c(0, 0.25, 0.5, 0.75, 1))

# Compute the interquartile range
IQR(x)


# Graphical representations of numerical Data

dataset$math.score
summary(dataset$math.score)

# Use dotchart to represent the data
dotchart(math.score, xlab = "Math Score")

# A bartplot showing the frequencies of math score
table(math.score)
barplot(table(math.score),
        col = "cyan", xlab = "Math Score", ylab = "# of times", 
        main = " Barplot of Math Score") 

# A histogram shows the frequency of math.score for particular interval
head(math.score)
hist(math.score)
hist(math.score, col=hcl(0), ylim=c(0,300))

# A boxplot for to display five-number summary of the data
x <- dataset$math.score

boxplot(x, horizontal = TRUE, xaxt = "n")
axis(side = 1, at = fivenum(x), labels = TRUE)

# The lower and upper ends of outlier ranges
f<- fivenum(x)
f
c(f[2]-1.5*(f[4]-f[2]),
  f[4]+1.5*(f[4]-f[2]))

# Boxplot for the five-number summary
boxplot(x, col = hcl(0), xlab= "Math Scores" , horizontal = TRUE)

boxplot(x, col=hcl(0), xaxt = "n",
        xlab = "Math Scores", horizontal = TRUE)
axis(side = 1, at = fivenum(x), labels = TRUE, las=2)
text(fivenum(x), rep(1.2,5), srt=90, adj=0,
     labels=c("Min","Lower Hinge", "Median", "Upper Hinge", "Max"))


# Analysis for at least one set of two or more variables

# Two-way table by adding the marginal distributions
x<-table(dataset$gender, dataset$race.ethnicity)
addmargins(x)

# The proportions for the gender and race dataset along the columns are shown
prop.table(x)

# Graphical summarization of two-way tables

# Mosaic plot
mosaicplot(x, color=c("red", "blue"), main='Mosaic plot of Race.ethnicity and Gender')

# Barplot

barplot(x, xlab = "Race.ethnicity",
        beside = TRUE, legend.text = TRUE,
        main = "Barplot of Race.ethnicity and Gender",
        ylim=c(0,200), col=c("red", "blue"))

t(x)

barplot(t(x), xlab = "Gender", 
        legend.text = TRUE,
        main = "Barplot of Race.ethnicity and Gender",
        ylim=c(0, 600), col=c("red", "blue", "yellow", "green", "orange"))


barplot(t(x), xlab = "Gender", 
        beside = TRUE, legend.text = TRUE,
        main = "Race.ethnicity", border=FALSE,
        args.legend = list(x = "center"),
        ylim=c(0,300), col = rainbow(5)) 


# Pick one variable with numerical data and examine the distribution of the data
fivenum(dataset$reading.score)
hist(dataset$reading.score, main="Histogram of reading score", xlab = "reading scores" ,col = rainbow(8))

boxplot(dataset$reading.score, col = hcl(0), xaxt = "n", main = "Boxplot of reading scores",
        xlab = "reading scores", horizontal = TRUE)
axis(side = 1, at = fivenum(dataset$reading.score), labels = TRUE, las=2)


# Draw various random samples of the data and show the applicability of the Central Limit Theorem for this variable

library(prob)
set.seed(101)

par(mfrow=c(1,1))
hist(dataset$writing.score, prob=TRUE, main="Histogram of writing score", breaks = 15, xlab = "Writing scores")

# Mean population
sample.mean <- mean(dataset$writing.score) 
sample.mean

# Standard population
sample.sd <- sd(dataset$writing.score)
sample.sd

#Draw 1000 samples of this data of size 10, show the histogram of the densities of the sample means. Compute the mean of the sample means and the standard deviation of the sample means 

samples<-1000
sample.size <- 10

xbar <- numeric(samples)

for (i in 1:samples) {
  xbar[i] <- mean(sample(dataset$writing.score, sample.size))
}

par(mfrow=c(1,1))
hist(xbar, prob = TRUE, breaks = 15, main = "Histogram of Size 10 Writing Scores")



sample.mean10 <-mean(xbar)
sample.mean10

sample.sd10 <- sd(xbar)
sample.sd10


#Draw 1000 samples of this data of size 20, show the histogram of the densities of the sample means. Compute the mean of the sample means and the standard deviation of the sample means

sample.size <- 20

xbar <- numeric(samples)
for (i in 1:samples) {
  xbar[i] <- mean(sample(dataset$writing.score, sample.size))
}

par(mfrow=c(1,1))
hist(xbar, prob = TRUE, breaks = 15 , main = "Histogram of Size 20 Writing Scores")


sample.mean20 <-mean(xbar)
sample.mean20

sample.sd20 <- sd(xbar)
sample.sd20

# Draw 1000 samples of this data of size 30, show the histogram of the densities of the sample means. Compute the mean of the sample means and the standard deviation of the sample means

sample.size <- 30

xbar <- numeric(samples)
for (i in 1:samples) {
  xbar[i] <- mean(sample(dataset$writing.score, sample.size))
}

par(mfrow=c(1,1))
hist(xbar, prob = TRUE, breaks = 15, main = "Histogram of Size 30 Writing Scores")

sample.mean30 <-mean(xbar)
sample.mean30

sample.sd30 <- sd(xbar)
sample.sd30


# Draw 1000 samples of this data of size 40, show the histogram of the densities of the sample means. Compute the mean of the sample means and the standard deviation of the sample means

sample.size <- 40

xbar <- numeric(samples)
for (i in 1:samples) {
  xbar[i] <- mean(sample(dataset$writing.score, sample.size))
}

par(mfrow=c(1,1))
hist(xbar, prob = TRUE, breaks = 15, main = "Histogram of Size 40 Writing Scores")

sample.mean40 <-mean(xbar)
sample.mean40

sample.sd40 <- sd(xbar)
sample.sd40


# Compare of means and standard deviations of the above four distributions

data.info <- data.frame(mean = c(sample.mean, sample.mean10, sample.mean20, sample.mean30, sample.mean40),
                        sd = c(sample.sd, sample.sd10, sample.sd20, sample.sd30, sample.sd40))
data.info


# From the effect of the sample size on the shape of distribution of the sample means. As the sample size increase,
# the spread of the distribution becomes narrower


# Show how various sampling methods can be used on your data

library(sampling)
set.seed(123)

# Sampling method #1 (Simple Random Sampling )  

s <- srswor(20,nrow(dataset))
sample.1 <- dataset[s!=0, ]
head(sample.1)

# Show the frequecy of the writing score variable
table(sample.1$writing.score)

hist(sample.1$writing.score, col=hcl(0), main = "Sampling Method #1: SRSWOR For Writing Score ", xlab = "writing score")


# Sampling method #2 (Systematic Sampling)

N <- nrow(dataset)
n <- 20

k <- ceiling(N/n)
k

r <- sample(k,1)
r

# Select every kth item 
seq(r, by = k, length = n)
sample.2 <- dataset[s, ]
head(sample.2)

# Show the frequency of writing score variable
table(sample.2$writing.score)

hist(sample.2$writing.score, col = hcl(0), main = "Sampling Method #2: Systematic Sampling For Writing Score", xlab = "writing score")

# Sampling method #3 (Stratified Sampling)

# Show the frequency of gender variable
freq <- table(dataset$gender)
freq

st.sizes <- 20 * freq/sum(freq)
st.sizes

st <- strata(dataset, stratanames = c("gender"),
             size = st.sizes, method = "srswor",
             description = TRUE)
st


st.sample.3 <- getdata(dataset,st)
st.sample.3

# Show the frequencies for each gender
table(st.sample.3$gender)


# Drawing conclusions: if these samples are used instead of the whole dataset, as the sample size increase, 
# the mean of sample mean distributions will get closer to the mean of the parent data 
# and the standard deviation of the sample mean distribution will decrease. When larger sample sizes,
# the distribution of the sample means approching the shape of a normal distribution.

# In addition, with sampling, it can save our time consuming on our large dataset, 
# a sample is used by the suitable sampling methods or strategy can also yield valid and reliable information


# Implementation of any feature(s) not mentioned in the specification

# Scatter plot of math socre and reading score variable
plot(dataset$math.score, dataset$reading.score, col= "blue",
     main = "Scatter plot: math socre vs reading score", xlab = "math score", ylab = "reading score")

# Scatter plot of math socre and writing score variable
plot(dataset$math.score, dataset$writing.score, col = "black",
     main = "Scatter plot: math socre vs writing score", xlab = "math score", ylab = "writing score")

# Scatter plot of reading socre and writing score varaible
plot(dataset$reading.score, dataset$writing.score, col = "cyan",
     main = "Scatter plot: reading socre vs writting score", xlab = "reading score", ylab = "writing score")






