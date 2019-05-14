### EXPLORATORY ANALYSIS ###
options(scipen = 999) # drop scientific notation
library(dplyr) # for the pipe operator (%>%)
library(ggplot2)
library(survival)
library(pander)
library(gridExtra)
panderOptions('knitr.auto.asis', FALSE)
PATH <- "Health Care/Project/"
datavalv <- read.table(paste0(PATH, "valvola.aortica.txt"), na.strings = ".", 
                       header = T, row.names = NULL)
names(datavalv)
# Cambia l'ordine di lv 
datavalv$lv <- ifelse(datavalv$lv == 1, 3, ifelse(datavalv$lv == 3, 1, 2))
# Rename and convert to factors 
datavalv.fac <- within(datavalv, {
  sex <- factor(sex, labels = c("male", "female"))
  con.cabg <- factor(con.cabg, labels = c("no", "si"))
  lv <- factor(lv, labels = c("scarsa", "moderata", "buona"), ordered = TRUE)
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
cat

varimp <- list() #create an empty list
i = 1
for (var in cat) {
  nlevels <- nlevels(d[,var])
  formula <- as.formula(paste("Surv(time,status) ~", var))
  fit.uni <- coxph(formula, data = d)
  beta <- coef(fit.uni)
  se   <- sqrt(diag(fit.uni$var))
  CI   <- round(exp(confint(fit.uni)), 4)

  for (lev.idx in 2:nlevels-1) {
    hr <- exp(beta[lev.idx])
    lowCI <- CI[lev.idx, 1]
    highCI <- CI[lev.idx, 2]
    pvalue <- 1 - pchisq((beta[lev.idx] / se[lev.idx]) ^ 2, nlevels - 1)
    feature <- names(beta[lev.idx])
    vec <- c(hr, lowCI, highCI, pvalue, feature)
    varimp[[i]] <- vec
    i = i + 1
  }
  #out
}
df <- do.call("rbind", varimp) #combine all vectors into a matrix
df <- data.frame(df, stringsAsFactors=FALSE)
colnames(df) <- c("HR", "lower95%CI", "upper95%CI", "pvalue", "feature")
varimp <- round(as.data.frame(sapply(df[, names(df) != "feature"], as.numeric)), 4)
varimp$feature <- df$feature
