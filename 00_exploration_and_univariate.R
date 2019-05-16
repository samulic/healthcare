##########################
## EXPLORATORY ANALYSIS ##
##########################
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

# Rimuovi record sbagliati con creatinina estremamente alta
datavalv <- datavalv[-which(datavalv$creat > 400), ]

# Inverti l'ordine di EF (fraz. eiezione) ventricolare
datavalv$lv <- ifelse(datavalv$lv == 1, 3, ifelse(datavalv$lv == 3, 1, 2))
# Crea un dataset con le variabili trasformate in factor
datavalv.fac <- within(datavalv, {
  sex <- factor(sex, labels = c("male", "female"))
  con.cabg <- factor(con.cabg, labels = c("no", "si"))
  lv <- factor(lv, labels = c("scarsa", "moderata", "buona"), ordered = TRUE)
  sten.reg.mix <- factor(sten.reg.mix, labels = c("stenosi", "rigurgito", "misto"))
})

# Crea un dataset senza le molteplici misurazioni nel tempo 
datavalvWide <- datavalv[!duplicated(datavalv$num), ]
datavalvWide.fac <- datavalv.fac[!duplicated(datavalv.fac$num), ]
head(datavalvWide.fac)

# Choose which dataset to use (w/ or w/o factors)
d <- datavalvWide.fac

pander(head(d), big.mark = ",")

VAR_NUMERIC <- c("age", "log.lvmi", "creat", "fuyrs")

pander(summary(d[, VAR_NUMERIC]), big.mark = ",") #-- statistiche descrittive

pander(cor(d[,VAR_NUMERIC]),big.mark=",") #-- matrice di correlazione

plot(d[,VAR_NUMERIC],pch=19,cex=.5) #-- scatter plot multivariato

par(mfrow = c(2,2))
for(i in VAR_NUMERIC){
  boxplot(d[,i], main = i, col = "lightblue", ylab = i, add = TRUE)
  points(mean(d[,i], pch = 23, col = "red"))
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
  theme(axis.ticks.x = element_blank(), axis.title.x = element_blank(),
        axis.text = element_text(size = 15, angle = 30), 
        axis.title = element_text(size = 18), plot.title = element_text(size = 30)) +
  ggtitle("Occorrenza dei livelli per variabili categoriche")

p2 <- ggplot(d, aes(x = lv)) + 
  geom_bar(fill = "blue", width = 0.8) + 
  coord_flip() + 
  xlab("EF ventr. sx pre-op.") + ylab("Number of Observations") +
  scale_y_continuous(limits = c(x_min, x_max), breaks = seq(x_min, x_max, by = 25)) +
  theme(axis.ticks.x = element_blank(), axis.title.x = element_blank(),
        axis.text = element_text(size = 15, angle = 30), axis.title = element_text(size = 18))

p3 <- ggplot(d, aes(x = sten.reg.mix)) + 
  geom_bar(fill = "#009fff", width = 0.8) + 
  coord_flip() + 
  xlab("Emodinamica valv. aortica") + #ylab("Number of Observations") +
  scale_y_continuous(limits = c(x_min, x_max), breaks = seq(x_min, x_max, by = 25)) +
  theme(axis.ticks.x = element_blank(), axis.title.x = element_blank(), 
        axis.title = element_text(size = 18), axis.text = element_text(size = 15, angle = 30))
  
p4 <- ggplot(d, aes(x = con.cabg)) + 
  geom_bar(fill = "#00d2ff", width = 0.8) + 
  coord_flip() + 
  xlab("Bypass coronarico") + ylab("Conteggio osservazioni") +
  scale_y_continuous(limits = c(x_min, x_max), breaks = seq(x_min, x_max, by = 25)) +
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 15, angle = 30))
grid.arrange(p1, p2, p3, p4, ncol = 1, heights = c(0.8, 1, 1, 0.8))


# Density for each level of factor variable
ggplot(d, aes(x = fuyrs))  +
  #geom_histogram(alpha = 0.8, binwidth = 1/4) + 
  geom_density() + 
  xlab("Follow up (years)") + ylab("Densita' di mortalita'") +
  ggtitle("Distribuzione del tempo di follow up", 
          subtitle = "Frazione di eiezione ventricolare sinistra pre-operatoria") +
  facet_wrap(~ lv, ncol = 1) +
  theme_light() + 
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), 
        title = element_text(size = 18))
ggplot(d, aes(x = fuyrs))  +
  #geom_histogram(alpha = 0.8, binwidth = 1/4) + 
  geom_density() + 
  xlab("Follow up (years)") +
  ggtitle("Factor level distribution", subtitle = "Emodinamica valvola aortica") + 
  facet_wrap(~ sten.reg.mix, ncol = 3)
ggplot(d, aes(x = fuyrs))  +
  #geom_histogram(alpha = 0.8, binwidth = 1/4) + 
  geom_density() + 
  xlab("Follow up (years)") +
  ggtitle("Factor level distribution", subtitle = "Bypass coronarico concomitante") + 
  facet_wrap(~ con.cabg, ncol = 3)

############################
## UNIVARIATE ASSOCIATION ##
############################
# Identify factor variables
#is.cat <- sapply(d, is.factor)
#is.cat 
# if we chose dataset without factor transformation
#if (sum(sapply(d, is.factor)) < 1) {
#  cat <- c("sex", "con.cabg", "lv", "sten.reg.mix") # ATTENZIONE ! Hard coded variables
#} else {
#  cat <- colnames(d[, is.cat]) 
#}
#cat
variables <- names(d) 
variables <- variables[!variables %in% c("num", "time", "fuyrs", "status", "log.lvmi")]
# Create dataframe of univariate models' (coefficients est. and pvalues)
varimp <- list() # create an empty list
i = 1 # index to assign data to the list
for (var in variables) {
  nlevels <- nlevels(d[, var]) # number of levels of the variable
  
  formula <- as.formula(paste("Surv(fuyrs, status) ~", var))
  fit.uni <- coxph(formula, data = d)
  
  beta <- coef(fit.uni)
  se   <- sqrt(diag(fit.uni$var))
  CI   <- exp(confint(fit.uni))
  if (nlevels > 0) {
    for (lev.idx in 1:(nlevels - 1)) {
      hr      <- exp(beta[lev.idx])
      lowCI   <- CI[lev.idx, 1]
      highCI  <- CI[lev.idx, 2]
      pvalue  <- 1 - pchisq((beta[lev.idx] / se[lev.idx]) ^ 2, nlevels - 1)
      feature <- names(beta[lev.idx])
      vec     <- c(hr, lowCI, highCI, pvalue, feature)
      varimp[[i]] <- vec
      i = i + 1
    }
  } else {
    hr      <- exp(beta)
    lowCI   <- CI[1]
    highCI  <- CI[2]
    pvalue  <- summary(fit.uni)$coefficients[5]
    feature <- names(coef(fit.uni))[1]
    vec     <- c(hr, lowCI, highCI, pvalue, feature)
    varimp[[i]] <- vec
    i = i + 1
  }
  #ifelse()
}
df <- do.call("rbind", varimp) #combine all vectors into a matrix
df <- data.frame(df, stringsAsFactors = FALSE)
colnames(df) <- c("HR", "lower95%CI", "upper95%CI", "pvalue", "feature")
# arrotonda
varimp <- round(as.data.frame(sapply(df[, names(df) != "feature"], as.numeric)), 4)
varimp$feature <- df$feature
(varimp <- varimp[order(varimp$pvalue), ])
