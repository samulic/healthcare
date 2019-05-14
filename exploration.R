### EXPLORATORY ANALYSIS ###
options(scipen = 999) # drop scientific notation
library(dplyr) # for the pipe operator (%>%)
library(pander)
library(gridExtra)
panderOptions('knitr.auto.asis', FALSE)
PATH <- "Health Care/Project/"
datavalv <- read.table(paste0(PATH, "valvola.aortica.txt"), na.strings = ".", 
                       header = T, row.names = NULL)
names(datavalv)
# Rename and convert to factors 
datavalv.fac <- within(datavalv, {
  sex <- factor(sex, labels = c("male", "female"))
  con.cabg <- factor(con.cabg, labels = c("no", "si"))
  lv <- factor(lv, labels = c("buona", "moderata", "scarsa"), ordered = T)
  sten.reg.mix <- factor(sten.reg.mix, labels = c("stenosi", "rigurgito", "misto"))
})

datavalvWide <- datavalv[!duplicated(datavalv$num), ]
datavalvWide.fac <- datavalv.fac[!duplicated(datavalv.fac$num), ]
head(datavalvWide.fac)

# Choose which dataset to use (w/ or w/o factors)
d <- datavalvWide.fac

pander(head(d), big.mark = ",")

VAR_NUMERIC <- c("age", "log.lvmi", "creat")

pander(summary(d[, VAR_NUMERIC]), big.mark = ",") #-- statistiche descrittive

pander(cor(d[,VAR_NUMERIC]),big.mark=",") #-- matrice di correlazione

plot(d[,VAR_NUMERIC],pch=19,cex=.5) #-- scatter plot multivariato

par(mfrow = c(1,3))
for(i in VAR_NUMERIC){
  boxplot(d[,i], main = i, col = "lightblue", ylab = i)
}
par(mfrow = c(1,1))

# X-axis min and max counts
x_min <- 0
x_max <- 185
p1 <- ggplot(d, aes(x = sex)) + 
  geom_bar(fill = "darkblue", width = 0.8) + 
  coord_flip() + 
  xlab("Genere") + ylab("Number of Observations") +
  scale_y_continuous(limits = c(x_min, x_max), breaks = seq(x_min, x_max, by = 25)) +
  #geom_text(aes(y = scales::percent(..count..) * 100)) +
  theme(axis.ticks.x = element_blank(), axis.title.x = element_blank()) +
  ggtitle("Factor variables levels' occurence")

p2 <- ggplot(d, aes(x = lv)) + 
  geom_bar(fill = "blue", width = 0.8) + 
  coord_flip() + 
  xlab("EF ventricolare sinistra pre-operatoria") + ylab("Number of Observations") +
  scale_y_continuous(limits = c(x_min, x_max), breaks = seq(x_min, x_max, by = 25)) +
  theme(axis.ticks.x = element_blank(), axis.title.x = element_blank())

p3 <- ggplot(d, aes(x = sten.reg.mix)) + 
  geom_bar(fill = "lightblue", width = 0.8) + 
  coord_flip() + 
  xlab("Emodinamica della valvola aortica") + ylab("Number of Observations") +
  scale_y_continuous(limits = c(x_min, x_max), breaks = seq(x_min, x_max, by = 25)) 
grid.arrange(p1, p2, p3, ncol = 1, heights = c(0.8, 1, 1))


# Density for each level of factor variable
ggplot(d, aes(x = fuyrs))  +
  #geom_histogram(alpha = 0.8, binwidth = 1/4) + 
  geom_density() + 
  xlab("Follow up (years)") +
  ggtitle("Factor level distribution", subtitle = "EF ventricolare sx pre-operatoria") +
  facet_wrap(~ lv, ncol = 3)
ggplot(d, aes(x = fuyrs))  +
  #geom_histogram(alpha = 0.8, binwidth = 1/4) + 
  geom_density() + 
  xlab("Follow up (years)") +
  ggtitle("Factor level distribution", subtitle = "Emodinamica valvola aortica") + 
  facet_wrap(~ sten.reg.mix, ncol = 3)

## Evaluate univariate association of factor variables to outcome
is.cat <- sapply(d, is.factor)
is.cat # doesn't work as expected if we chose dataset without factor transformation

cat <- d[, is.cat] %>% colnames()
output<-lapply(cat.twolev, function(var) {
  formula    <- as.formula(paste("Surv(time,status) ~ ", var))
  fit.uni <- coxph(formula, data = d)
  beta <- coef(fit.uni)
  se   <- sqrt(diag(fit.uni$var))
  CI   <- round(exp(confint(fit.uni)), 3)
  round(c(exp(beta), CI, p = 1 - pchisq((beta / se) ^ 2, nlevels(d[,var]) - 1)), 4)
})
results <- as.data.frame(matrix(unlist(output), ncol = 4, byrow = T))
names(results) <- c("HR","lower95%CI","upper95%CI","p")
results$features <- cat.twolev
results

output<-lapply(cat, function(var) {
  nlevels <- nlevels(d[,var])
  formula <- as.formula(paste("Surv(time,status) ~ ", var))
  fit.uni <- coxph(formula, data = d)
  beta <- coef(fit.uni)
  se   <- sqrt(diag(fit.uni$var))
  CI   <- round(exp(confint(fit.uni)), 3)
  
  if (nlevels > 2) {
    out <- data.frame()
    for (lev.idx in 1:nlevels-1) {
      out <- rbind(out, round(c(exp(beta[lev.idx]), CI[lev.idx, ], 
                                p = 1 - pchisq((beta[lev.idx] / se[lev.idx]) ^ 2, nlevels - 1)), 4))
    }
  } else {
    out <- round(c(exp(beta), CI, p = 1 - pchisq((beta / se) ^ 2, 1)), 4)
  }
  out
})

#output<-lapply(cat, function(var) {
out <- data.frame()
colnames(out) <- c("HR","lower95%CI","upper95%CI","p")
for (var in cat) {
  nlevels <- nlevels(d[,var])
  formula <- as.formula(paste("Surv(time,status) ~ ", var))
  fit.uni <- coxph(formula, data = d)
  beta <- coef(fit.uni)
  se   <- sqrt(diag(fit.uni$var))
  CI   <- round(exp(confint(fit.uni)), 4)
  print(CI)
  for (lev.idx in 1:nlevels-1) {
    out <- rbind(out, round(
      c(exp(beta[lev.idx]), CI[lev.idx, ], 
        p = 1 - pchisq((beta[lev.idx] / se[lev.idx]) ^ 2, nlevels - 1)), 
      4))
  }
  #out
}
colnames(out) <- c("HR","lower95%CI","upper95%CI","p")



results <- as.data.frame(matrix(unlist(output), ncol = 4, byrow = T))
names(results) <- c("HR","lower95%CI","upper95%CI","p")
results$features <- cat.twolev
results

