#This is a test file for Git use

library(swirl)
install_course("Statistical Inference")
install_course("Advanced R Programming")

### MR practicals
library(ivpack)
coursedata <- read.csv("J:/MR cambridge course/coursedata.csv")
#continuous exposure X, continuous outcome Y, binary outcome Y.bin

head(coursedata)
attach(coursedata)
#Continuous outcome [ratio method]
by1 <- lm(y~g1)$coef[2]
bx1 <- lm(x~g1)$coef[2]
(beta.ratio1=by1/bx1)

#standard error - first order
byse1 <- summary(lm(y~g1))$coef[2,2]
(se.ratio1first <- byse1/sqrt(bx1^2))
#standard error - second order
bxse1 <- summary(lm(x~g1))$coef[2,2]
(se.ratio1second <- sqrt(byse1^2/bx1^2 + by1^2*bxse1^2/bx1^4))
#F-statistic
(fstat1 <- summary(lm(x~g1))$f[1])

ratio_estimate<-function(g, x, y){
  by = lm(y~g)$coef[2]
  byse = summary(lm(y~g))$coef[2,2]
  bx = lm(x~g)$coef[2]
  bxse = summary(lm(x~g))$coef[2,2]
  beta.ratio = by/bx
  se.ratiofirst = byse/sqrt(bx^2)
  se.ratiosecond = sqrt(byse^2/bx^2 + by^2*bxse^2/bx^4)
  fstat = summary(lm(x~g))$f[1]
  MAF = (sum(g==1) + 2*sum(g==2))/(2*length(g))
  return(c(by, byse, bx, bxse, beta.ratio, se.ratiofirst, se.ratiosecond,
           fstat, MAF))
}
g_mat=cbind(g1,g2,g3,g4)
ratio.all<-apply(g_mat,2,ratio_estimate, x=x,y=y)
row.names(ratio.all)<-c("by","byse","bx","bxse","beta.ratio","se.ratio.first",
                        "se.ratio.second","fstat","MAF")
ratio.all

