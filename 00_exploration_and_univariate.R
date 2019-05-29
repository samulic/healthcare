options(scipen = 999) # drop scientific notation
library(dplyr) # for the pipe operator (%>%)
library(ggplot2)
library(survival)
library(pander)
library(gridExtra)

panderOptions('knitr.auto.asis', FALSE)
# Set PATH alla cartella del progetto
PATH <- "Health Care/Project/"

datavalv <- read.table(paste0(PATH, "valvola.aortica.txt"), na.strings = ".", 
                       header = T, row.names = NULL)
#######################
######### EDA #########
#######################
names(datavalv)

## PREPROCESSING - start
# Funzione che rimuove, qualora presenti, i record sbagliati 
# con creatinina estremamente alta
removeOutliers <- function(data) {
  outlier.idx <- which(data$creat > 300)
  if (length(outlier.idx) > 0) {
    data <- data[-outlier.idx, ]
  }
  data
}
# Inverti l'ordine di EF (fraz. eiezione) ventricolare
datavalv$lv <- ifelse(datavalv$lv == 1, 3, ifelse(datavalv$lv == 3, 1, 2))
# Crea un dataset con le variabili trasformate in factor
datavalv.fac <- within(datavalv, {
  sex <- factor(sex, labels = c("male", "female"))
  con.cabg <- factor(con.cabg, labels = c("no", "si"))
  lv <- factor(lv, labels = c("scarsa", "moderata", "buona"), ordered = TRUE)
  sten.reg.mix <- factor(sten.reg.mix, labels = c("stenosi", "rigurgito", "misto"))
})
# Crea un dataset senza le misurazioni nel tempo, con e senza factor
datavalvWide <- datavalv[!duplicated(datavalv$num), ]
datavalvWide.fac <- datavalv.fac[!duplicated(datavalv.fac$num), ]
head(datavalvWide.fac)
## PREPROCESSING - end

# Scegli il dataset da usare da questo punto in avanti
# con o senza fattori espliciti (datavalv* o datavalv*.fac)
# con o senza cov. longitudinali (datavalv* o datavalvWide*)
d <- datavalvWide.fac # con factors e senza misure ripetute nel tempo
pander(head(d), big.mark = ",")
# Scegli le covariate numeriche da analizzare
VAR_NUMERIC <- c("age", "log.lvmi", "creat", "fuyrs")
# Statistiche descrittive
pander(summary(d[, VAR_NUMERIC]), big.mark = ",")
# Matrice di correlazione
pander(cor(d[,VAR_NUMERIC]),big.mark=",") 
# Scatter plot multivariato
plot(d[,VAR_NUMERIC],pch=19,cex=.5) 
# Box-plots
par(mfrow = c(1,4))
for(i in VAR_NUMERIC){
  # Color of outliers
  outcol <- "black" # black e' il default, non si distingue dalle altre obs.
  #if (i == "creat") outcol <- "red"
  boxplot(d[,i], main = i, col = "lightblue", outcol=outcol, ylab = i)
#  points(mean(d[,i], pch = 23, col = "red"))
}
par(mfrow = c(2,2))
for(i in VAR_NUMERIC){
  hist(d[, i], main = i, col = "lightblue", xlab = i, freq = F)
}
# Adesso plot senza outlier
d <- removeOutliers(d)
for(i in VAR_NUMERIC){
  hist(d[,i],main=i,col="lightblue",xlab=i,freq=F)
}
par(mfrow = c(1,4))
for(i in VAR_NUMERIC){
  boxplot(d[,i], main = i, col = "lightblue", outcol=outcol, ylab = i)
}
par(mfrow = c(1,1))

# Plotta gli istogrammi per le variabili categoriche 
# conta le occorrenze per ogni livello di ogni var.
# X-axis min and max counts, MAX 255 pazienti
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


# Distribuzione del tempo di follow up per ogni livello delle cov. categoriche
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

#############################
## ASSOCIAZIONE UNIVARIATA ##
#############################
variables <- names(d) 
# Escludi alcune variabili
variables <- variables[!variables %in% c("num", "time", "fuyrs", "status", "log.lvmi")]
# Create an empty list of univariate models' (coefficients est. and pvalues)
fits.uni <- list() # 
i = 1 # index to assign results to the list
for (var in variables) {
  nlevels <- nlevels(d[, var]) # number of levels of the variable
  # Prepara la formula per il modello univariato cambiando la var. esplicativa
  formula <- as.formula(paste("Surv(fuyrs, status) ~", var))
  fit.uni <- coxph(formula, data = d)
  # Ottiene i coefficienti e gli intervalli di confidenza
  beta <- coef(fit.uni)
  se   <- sqrt(diag(fit.uni$var))
  CI   <- exp(confint(fit.uni))
  # Distingui tra covariate numeriche (nlevels = 0) e quelle categoriche
  # per gestire le differenze nel processamento degli output
  if (nlevels > 1) { # zero or more than two levels
    # I coefficienti che si ottengono con covariate cat. sono `nlevels - 1`
    # Quindi cicla per ottenere tutte i possibili coefficienti 
    # se nlevels = 2 allora una solo esecuzione del for, ovviamente
    for (lev.idx in 1:(nlevels - 1)) {
      hr      <- exp(beta[lev.idx])
      lowCI   <- CI[lev.idx, 1]
      highCI  <- CI[lev.idx, 2]
      pvalue  <- 1 - pchisq((beta[lev.idx] / se[lev.idx]) ^ 2, nlevels - 1)
      feature <- names(beta[lev.idx])
      vec     <- c(hr, lowCI, highCI, pvalue, feature)
      fits.uni[[i]] <- vec
      i = i + 1
    }
  } else {
    hr      <- exp(beta)
    lowCI   <- CI[1]
    highCI  <- CI[2]
    pvalue  <- summary(fit.uni)$coefficients[5]
    feature <- names(coef(fit.uni))[1]
    vec     <- c(hr, lowCI, highCI, pvalue, feature)
    fits.uni[[i]] <- vec
    i = i + 1
  }
  #ifelse()
}
#fits.uni
# Combina tutti i vettori della lista in una matrice
df <- do.call("rbind", fits.uni) 
# Trasforma la matrice in dataframe
df <- data.frame(df, stringsAsFactors = FALSE)
# Setta il nome delle colonne
colnames(df) <- c("HR", "lower95%CI", "upper95%CI", "pvalue", "feature")
# Arrotonda tutti i numeri alla quarta cifra decimale 
# Converti prima a numero, eslcudendo la colonna di stringhe
varimp <- round(as.data.frame(sapply(df[, names(df) != "feature"], as.numeric)), 4)
# Riaggiungi la colonna del nome delle variabili
varimp$feature <- df$feature
# Ordina per nome
(varimp <- varimp[order(varimp$feature), ])

#### PLOT Double-check dell'analisi univariata ####
# d e' il dataframe senza le misurazioni ripetute nel tempo

##-- Presenza o no di bypass --##
# Costruisci dataframe per i pazienti deceduti 
# raggruppando per ogni anno di follow-up anche per bypass
df <- d %>% group_by(tempofu = round(fuyrs, 0), con.cabg) %>% 
  summarise(mort = sum(status), 
            nonmort = sum(abs(status - 1)),
            tot = n())
# Conta quanti pazienti in totale hanno subito bypass e quanti no
df$tot_bypass <- ave(df$tot, df$con.cabg, FUN = sum)
# Suddividi il dataframe
idx_bypass <- which(df$con.cabg == "si")
df_bypass_si <- df[idx_bypass,]
df_bypass_no <- df[-idx_bypass, ]
# Tieni conto del numero totale di pazienti ad ogni anno di follow up
df_bypass_si$tot_cumsum <- cumsum(df_bypass_si$tot)
df_bypass_no$tot_cumsum <- cumsum(df_bypass_no$tot)
# Unisci i due dataset
dd <- rbind(df_bypass_no, df_bypass_si)

dd <- dd %>% group_by(con.cabg) %>% mutate(diff = tot_bypass-lag(tot_cumsum))

# coalesce unisce due colonne sostituendo i NA della colonna n.1 con la n.2
dd <- dd %>% mutate(left_alive = coalesce(diff, tot_bypass)) %>% 
  select(tempofu, con.cabg, mort, tot, left_alive)
# Calcola la propozione di decessi
dd$mort_su_vivi <- dd$mort / dd$left_alive
# Rimuovi le righe con zero decessi
dd <- dd[dd$mort != 0,]
# Grafico 1 - bypass
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
plot(g1)

##-- Emodinamica della valvola aortica --##
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
