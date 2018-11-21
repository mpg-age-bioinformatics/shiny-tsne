setwd("~/Documents/corefacility/shiny_apps/shiny/LifeSpanCurves")

# this script is to develop the life span curves in R in general before putting the code into a shiny app

library(survival)
library(xlsx)

D <- read.xlsx('lifespan_test.xlsx', sheetIndex = 1, header = T)
head(D)

death = 4
censor = 5
day = 1
other = c(2,3)
num_var = length(other)
# reformat table to
#day status genotype food
# one line per even (death == 1 or censored == 0)
D.survival <- as.data.frame(matrix(0, nrow = sum(D[,c('death', 'censor')]), ncol = 2 + num_var))
names(D.survival) <- c('day', 'status', names(D)[2:3]) # this needs to be flexible!

r = 1 # row counter for D.survival
for(i in 1:nrow(D)){
  if(D[i, death] > 0){
    for(event in 1:D[i,death]){
      D.survival[r,c(1,2)] <- c(D[i,day], 1)
      D.survival[r, c(3:ncol(D.survival))] <- as.character(D[i, other])
      r = r+1
    }
  }
  if(D[i, censor] > 0){
    for(event in 1:D[i,censor]){
      D.survival[r,c(1,2)] <- c(D[i,day], 0)
      D.survival[r, c(3:ncol(D.survival))] <- as.character(D[i, other])
      r = r+1
    }
  }
}

fit <- survfit(Surv(day, status) ~ genotypes + food, data = D.survival)
plot(fit, col = rep(1:num_var, times = num_var), lty = rep(1:num_var, each = num_var), lwd = 1.5)
legend('topright', )
print(coxph(Surv(day,status)~genotypes*food,data=D.survival,method="breslow"))
