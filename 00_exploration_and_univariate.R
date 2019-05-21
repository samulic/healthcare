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

# Box-plots
par(mfrow = c(1,4))
for(i in VAR_NUMERIC){
  # Color of outliers
  outcol <- "black"
  #if (i == "creat") outcol <- "red"
  boxplot(d[,i], main = i, col = "lightblue", outcol=outcol, ylab = i)
#  points(mean(d[,i], pch = 23, col = "red"))
}
par(mfrow = c(2,2))
for(i in VAR_NUMERIC){
  hist(d[,i],main=i,col="lightblue",xlab=i,freq=F)
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
variables <- names(d) 
# Escludi alcune variabili
variables <- variables[!variables %in% c("num", "time", "fuyrs", "status", "log.lvmi")]
# Create an empty list of univariate models' (coefficients est. and pvalues)
varimp <- list() # 
i = 1 # index to assign data to the list
for (var in variables) {
  nlevels <- nlevels(d[, var]) # number of levels of the variable
  
  formula <- as.formula(paste("Surv(fuyrs, status) ~", var))
  fit.uni <- coxph(formula, data = d)
  
  beta <- coef(fit.uni)
  se   <- sqrt(diag(fit.uni$var))
  CI   <- exp(confint(fit.uni))
  if (nlevels > 1) { # zero or more than two levels
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
(varimp <- varimp[order(varimp$feature), ])

## PLOT Double-check dell'analisi univariata
# Presenza o no di bypass
df <- d %>% group_by(tempofu = round(fuyrs, 0), con.cabg) %>% 
  summarise(mort = sum(status), 
            nonmort = sum(abs(status - 1)),
            tot = n())
# Conta quanti pazienti in totale hanno subito bypass e quanti no
df$tot_bypass <- ave(df$tot, df$con.cabg, FUN = sum)

idx_bypass <- which(df$con.cabg == "si")
df_bypass_si <- df[idx_bypass,]
df_bypass_no <- df[-idx_bypass, ]
# Tieni conto del di pazienti ad ogni tempo di follow up
df_bypass_si$tot_cumsum <- cumsum(df_bypass_si$tot)
df_bypass_no$tot_cumsum <- cumsum(df_bypass_no$tot)
# unisci i due dataset
dd <- rbind(df_bypass_no, df_bypass_si)

dd <- dd %>% group_by(con.cabg) %>% mutate(diff = tot_bypass-lag(tot_cumsum))

# coalesce unisce due colonne sostituendo i NA della colonna n.1 con la n.2
dd <- dd %>% mutate(left_alive = coalesce(diff, tot_bypass)) %>% 
  select(tempofu, con.cabg, mort, tot, left_alive)

dd$mort_su_vivi <- dd$mort / dd$left_alive
# Rimuovi le righe con zero decessi
dd <- dd[dd$mort != 0,]

g1 <- ggplot(data = dd, aes(x = tempofu, y = mort_su_vivi, col = con.cabg, size = mort)) + 
  geom_point() + 
  geom_smooth(se = F) +
  xlab("Tempo di follow up (anni)") +
  ylab("Frazione di decessi sul totale") +
  ggtitle(label = "Andamenti delle proprozioni di decessi dopo l'operazione di impianto di valvole cardiache", 
          subtitle = "Distinti per bypass coronarico concomitante") +
  labs(col = "Bypass\ncoronarico", size = "Decessi") +
  scale_color_manual(values = c("#228B22", "red")) +
  guides(size = guide_legend(override.aes = list(linetype = 0)),
         color = guide_legend(override.aes = list(linetype = 0))) +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        title = element_text(size = 18.5)) 

# Emodinamica della valvola aortica
df <- d %>% group_by(tempofu = round(fuyrs, 0), sten.reg.mix) %>% 
  summarise(mort = sum(status), 
            nonmort = sum(abs(status - 1)),
            tot = n())

df$tot_bypass <- ave(df$tot, df$sten.reg.mix, FUN=sum)
idx_misto <- which(df$sten.reg.mix == "misto")
idx_rigurgito <- which(df$sten.reg.mix == "rigurgito")

df_misto     <- df[idx_misto,]
df_rigurgito <- df[idx_rigurgito,]
df_stenosi   <- df[-c(idx_misto, idx_rigurgito), ]

df_misto$tot_cumsum <- cumsum(df_misto_si$tot)
df_rigurgito$tot_cumsum <- cumsum(df_rigurgito$tot)
df_stenosi$tot_cumsum <- cumsum(df_stenosi$tot)

dd <- rbind(df_misto, df_rigurgito, df_stenosi)

dd <- dd %>% group_by(sten.reg.mix) %>% mutate(diff = tot_bypass - lag(tot_cumsum))

dd <- dd %>% mutate(left_alive = coalesce(diff, tot_bypass)) %>% 
  select(tempofu, sten.reg.mix, mort, tot, left_alive)

dd$mort_su_vivi <- dd$mort / dd$left_alive
# Rimuovi le righe con zero decessi
dd <- dd[dd$mort != 0,]

g2 <- ggplot(data = dd, aes(x = tempofu, y = mort_su_vivi, col = sten.reg.mix, size = mort)) + 
  geom_point() + 
  geom_smooth(se = F) +
  xlab("Tempo di follow up (anni)") +
  ylab("Frazione di decessi sul totale") +
  ggtitle(label = "",
    subtitle = "Distinti per emodinamica della valvola aortica") +
  labs(col = "Emodinamica\nvalv. aort.", size = "Decessi") +
  scale_color_manual(values = c("Blue", "Yellow", "green")) +
  guides(size = guide_legend(override.aes = list(linetype = 0)),
         color = guide_legend(override.aes = list(linetype = 0))) +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        title = element_text(size = 18.5)) 

grid.arrange(g1, g2, ncol = 1)
