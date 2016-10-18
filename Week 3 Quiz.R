## R Programming Week 3 Quiz

library(datasets)
data(iris)
names(iris)
str(iris)
## what is the mean of 'Sepal.Length' for the species virginica? 
tapply(iris$Sepal.Length,iris$Species,mean) 
## 6.588

## what R code returns a vector of the means of the variables 'Sepal.Length', 'Sepal.Width', 'Petal.Length', and 'Petal.Width'?
## 1. rowMeans(iris[,1:4])  -> gives mean of the rows
apply(iris[,1:4],2,mean) ## -> 1 is by row and 2 is by column
## 3. colMeans(iris)  -> can only be applied on numeric
## 4. apply(iris[,1:4],1,mean)  -> 1 is by row
## 5. apply(iris,1,mean)


## How can one calculate the average miles per gallon (mpg) by 
## number of cylinders in the car (cyl)? Select all that apply.
library(datasets)
data(mtcars)
str(mtcars)
## apply(mtcars,2,mean) -> 2 isby column
tapply(mtcars$mpg, mtcars$cyl, mean)
sapply(split(mtcars$mpg, mtcars$cyl), mean)
## mean(mtcars$mpg,mtcars$cyl)   -> error: 'trim' must be numeric of length one
## lapply(mtcars,mean)   -> returns a list with mean of columns
with(mtcars,tapply(mpg,cyl,mean))
## sapply(mtcars,cyl,mean)  -> Error in match.fun(FUN) : object 'cyl' not found
## split(mtcars,mtcars$cyl)  -> splits data based on the cyl values. No mean calculated


## what is the absolute difference between the average horsepower of 4-cylinder 
## cars and the average horsepower of 8-cylinder cars?
abs(mean(mtcars$hp[mtcars$cyl==4])-mean(mtcars$hp[mtcars$cyl==8]))

debug(ls)
ls()