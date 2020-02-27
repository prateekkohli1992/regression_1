---
title: "Modeling in R"
output: html_notebook
---


Load the libraries

```{r}
library(MASS)
library(tidyverse)
library(car)
library(perturb)
library(corpcor)
library(caret)
library(RANN)
library(bnstruct)
library(DMwR)
library(psych)
library(corrplot)
library(broom)
library(gridExtra)
library(grid)
library(arules)
```


Model 1 with all variables

```{r}
df <- read.csv('Data/data_train.csv')
x_axis1=df[c(1)]
df<-df[ -c(1) ]
mod1 <- lm(price~.+compressionratio*fueltype,data=df)
summary(mod1)
```
Lets check residual plots

```{r}
mod1_table <- augment(mod1)
head(mod1_table)
```

```{r}
par(mfrow=c(7,7))
for (col in colnames(df)){
if(is.numeric(df[col])){
    ggplot(mod1_table, aes(col,.std.resid))+geom_point()
}
  else{
    ggplot(mod1_table, aes(col,.std.resid))+geom_boxplot()
    
  }
}
```


```{r}
ggplot(mod1_table, aes(wheelbase,.std.resid))+geom_point()
ggplot(mod1_table, aes(carlength,.std.resid))+geom_point()
ggplot(mod1_table, aes(carwidth,.std.resid))+geom_point()
ggplot(mod1_table, aes(carheight,.std.resid))+geom_point()
ggplot(mod1_table, aes(curbweight,.std.resid))+geom_point()
ggplot(mod1_table, aes(enginesize,.std.resid))+geom_point()
ggplot(mod1_table, aes(boreratio,.std.resid))+geom_point()
ggplot(mod1_table, aes(stroke,.std.resid))+geom_point()
ggplot(mod1_table, aes(compressionratio,.std.resid))+geom_point()
ggplot(mod1_table, aes(horsepower,.std.resid))+geom_point()
ggplot(mod1_table, aes(peakrpm,.std.resid))+geom_point()
ggplot(mod1_table, aes(citympg,.std.resid))+geom_point()
ggplot(mod1_table, aes(highwaympg,.std.resid))+geom_point()
```

```{r}
ggplot(mod1_table) +
  aes(x = symboling, y = .std.resid) +
  geom_point() -> p1

ggplot(mod1_table) +
  aes(x = factor(symboling), y = .std.resid) +
  geom_boxplot() +
  geom_smooth(aes(group = 1), se = FALSE) -> p2

grid.arrange(p1, p2, ncol = 2)

ggplot(mod1_table) +
  aes(x = fueltype, y = .std.resid) +
  geom_point() -> p1

ggplot(mod1_table) +
  aes(x = factor(fueltype), y = .std.resid) +
  geom_boxplot() +
  geom_smooth(aes(group = 1), se = FALSE) -> p2

grid.arrange(p1, p2, ncol = 2)


ggplot(mod1_table) +
  aes(x = aspiration, y = .std.resid) +
  geom_point() -> p1

ggplot(mod1_table) +
  aes(x = factor(aspiration), y = .std.resid) +
  geom_boxplot() +
  geom_smooth(aes(group = 1), se = FALSE) -> p2

grid.arrange(p1, p2, ncol = 2)

ggplot(mod1_table) +
  aes(x = doornumber, y = .std.resid) +
  geom_point() -> p1

ggplot(mod1_table) +
  aes(x = factor(doornumber), y = .std.resid) +
  geom_boxplot() +
  geom_smooth(aes(group = 1), se = FALSE) -> p2

grid.arrange(p1, p2, ncol = 2)

ggplot(mod1_table) +
  aes(x = carbody, y = .std.resid) +
  geom_point() -> p1

ggplot(mod1_table) +
  aes(x = factor(carbody), y = .std.resid) +
  geom_boxplot() +
  geom_smooth(aes(group = 1), se = FALSE) -> p2

grid.arrange(p1, p2, ncol = 2)


ggplot(mod1_table) +
  aes(x = drivewheel, y = .std.resid) +
  geom_point() -> p1

ggplot(mod1_table) +
  aes(x = factor(drivewheel), y = .std.resid) +
  geom_boxplot() +
  geom_smooth(aes(group = 1), se = FALSE) -> p2

grid.arrange(p1, p2, ncol = 2)



ggplot(mod1_table) +
  aes(x = enginelocation, y = .std.resid) +
  geom_point() -> p1

ggplot(mod1_table) +
  aes(x = factor(enginelocation), y = .std.resid) +
  geom_boxplot() +
  geom_smooth(aes(group = 1), se = FALSE) -> p2

grid.arrange(p1, p2, ncol = 2)


ggplot(mod1_table) +
  aes(x = enginetype, y = .std.resid) +
  geom_point() -> p1

ggplot(mod1_table) +
  aes(x = factor(enginetype), y = .std.resid) +
  geom_boxplot() +
  geom_smooth(aes(group = 1), se = FALSE) -> p2

grid.arrange(p1, p2, ncol = 2)


ggplot(mod1_table) +
  aes(x = cylindernumber, y = .std.resid) +
  geom_point() -> p1

ggplot(mod1_table) +
  aes(x = factor(cylindernumber), y = .std.resid) +
  geom_boxplot() +
  geom_smooth(aes(group = 1), se = FALSE) -> p2

grid.arrange(p1, p2, ncol = 2)

ggplot(mod1_table) +
  aes(x = fuelsystem, y = .std.resid) +
  geom_point() -> p1

ggplot(mod1_table) +
  aes(x = factor(fuelsystem), y = .std.resid) +
  geom_boxplot() +
  geom_smooth(aes(group = 1), se = FALSE) -> p2

grid.arrange(p1, p2, ncol = 2)

ggplot(mod1_table) +
  aes(x = brandName, y = .std.resid) +
  geom_point() -> p1

ggplot(mod1_table) +
  aes(x = factor(brandName), y = .std.resid) +
  geom_boxplot() +
  geom_smooth(aes(group = 1), se = FALSE) -> p2

grid.arrange(p1, p2, ncol = 2)
```

