# Presenza o no di bypass
df <- d %>% group_by(tempofu = round(fuyrs, 0), con.cabg) %>% 
  summarise(mort = sum(status), 
            nonmort = sum(abs(status - 1)),
            tot = n())
            #frazmort = mort / tot)
df$tot_bypass <- ave(df$tot, df$con.cabg, FUN=sum)
idx_bypass <- which(df$con.cabg == "si")
df_bypass_si <- df[idx_bypass,]
df_bypass_no <- df[-idx_bypass, ]

df_bypass_si$tot_cumsum <- cumsum(df_bypass_si$tot)
df_bypass_no$tot_cumsum <- cumsum(df_bypass_no$tot)

dd <- rbind(df_bypass_no, df_bypass_si)

dd <- dd %>% group_by(con.cabg) %>% mutate(diff = tot_bypass-lag(tot_cumsum))

dd <- dd %>% mutate(left_alive = coalesce(diff, tot_bypass)) %>% select(tempofu, con.cabg, mort, tot, left_alive)

dd$mort_su_vivi <- dd$mort / dd$left_alive

ggplot(data = dd, aes(x = tempofu, y = mort_su_vivi, col = con.cabg, size = mort)) + geom_point() + geom_smooth(se = F)

# Presenza o no di stenosi mista a rigurgito
sten.reg.mix <- ifelse(d$sten.reg.mix == "misto", "misto", "nonmisto")
data <- d
data$sten.reg.mix <- sten.reg.mix

df <- data %>% group_by(tempofu = round(fuyrs, 0), sten.reg.mix) %>% 
  summarise(mort = sum(status), 
            nonmort = sum(abs(status - 1)),
            tot = n())
#frazmort = mort / tot)
df$tot_bypass <- ave(df$tot, df$sten.reg.mix, FUN=sum)
idx_misto <- which(df$sten.reg.mix == "misto")
df_misto_si <- df[idx_misto,]
df_misto_no <- df[-idx_misto, ]

df_misto_si$tot_cumsum <- cumsum(df_misto_si$tot)
df_misto_no$tot_cumsum <- cumsum(df_misto_no$tot)

dd <- rbind(df_misto_no, df_misto_si)

dd <- dd %>% group_by(sten.reg.mix) %>% mutate(diff = tot_bypass - lag(tot_cumsum))

dd <- dd %>% mutate(left_alive = coalesce(diff, tot_bypass)) %>% select(tempofu, sten.reg.mix, mort, tot, left_alive)

dd$mort_su_vivi <- dd$mort / dd$left_alive

ggplot(data = dd, aes(x = tempofu, y = mort_su_vivi, col = sten.reg.mix, size = mort)) + geom_point() + geom_smooth(se = F)

### Presenza o no di rigurgito
sten.reg.mix <- ifelse(d$sten.reg.mix == "rigurgito", "rigurgito", "nonrigurgito")
data <- d
data$sten.reg.mix <- sten.reg.mix

df <- data %>% group_by(tempofu = round(fuyrs, 0), sten.reg.mix) %>% 
  summarise(mort = sum(status), 
            nonmort = sum(abs(status - 1)),
            tot = n())
#frazmort = mort / tot)
df$tot_bypass <- ave(df$tot, df$sten.reg.mix, FUN=sum)
idx_misto <- which(df$sten.reg.mix == "rigurgito")
df_misto_si <- df[idx_misto,]
df_misto_no <- df[-idx_misto, ]

df_misto_si$tot_cumsum <- cumsum(df_misto_si$tot)
df_misto_no$tot_cumsum <- cumsum(df_misto_no$tot)

dd <- rbind(df_misto_no, df_misto_si)

dd <- dd %>% group_by(sten.reg.mix) %>% mutate(diff = tot_bypass - lag(tot_cumsum))

dd <- dd %>% mutate(left_alive = coalesce(diff, tot_bypass)) %>% select(tempofu, sten.reg.mix, mort, tot, left_alive)

dd$mort_su_vivi <- dd$mort / dd$left_alive

ggplot(data = dd, aes(x = tempofu, y = mort_su_vivi, col = sten.reg.mix, size = mort)) + geom_point() + geom_smooth(se = F)

