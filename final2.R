

#Age(Nominal)
## Age as numric
AgeNum05 <- as.character(HHS_2005$Age)
unique(AgeNum05)
AgeNum05 <- gsub("15-19","17", AgeNum05 )
AgeNum05 <- gsub("20-24","22", AgeNum05 )
AgeNum05 <- gsub("25-29","27", AgeNum05 )
AgeNum05 <- gsub("30-34","32", AgeNum05 )
AgeNum05 <- gsub("35-39","37", AgeNum05 )
AgeNum05 <- gsub("40-44","42", AgeNum05 )
AgeNum05 <- gsub("45-49","47", AgeNum05 )
AgeNum05 <- gsub("50-54","52", AgeNum05 )
AgeNum05 <- gsub("55-59","57", AgeNum05 )
AgeNum05 <- gsub("60-64","62", AgeNum05 )
AgeNum05 <- gsub("65-69","67", AgeNum05 )
AgeNum05 <- gsub("70-74","72", AgeNum05 )
AgeNum05 <- gsub("75+","77", AgeNum05 )
AgeNum05 <- gsub("UNSP","", AgeNum105 )
AgeNum05 <- as.numeric(AgeNum05)
AgeNum05 <- as.data.frame(AgeNum05)
summary(AgeNum05)


dat13$AgeNum13=AgeNum13$Age
sd(dat13$AgeNum13, na.rm= TRUE)
var(dat13$AgeNum13, na.rm= TRUE)

#LOS(Numaric)
## LOS as numric
LOSNum13 <- as.character(HHS_2013$LOS)
unique(LOSNum13)
LOSNum13 <- gsub("< 1","1", LOSNum13 )
LOSNum13 <- gsub("1-2","1.5", LOSNum13 )
LOSNum13 <- gsub("3-4","3.5", LOSNum13 )
LOSNum13 <- gsub("5-9","7", LOSNum13 )
LOSNum13 <- gsub("10-14","12", LOSNum13 )
LOSNum13 <- gsub("15-19","17", LOSNum13 )
LOSNum13 <- gsub("20-24","22", LOSNum13 )
LOSNum13 <- gsub("25-29","27", LOSNum13 )
LOSNum13 <- gsub("30-34","32", LOSNum13 )
LOSNum13 <- gsub("35+","37", LOSNum13 )
LOSNum13 <- gsub("UNSP","", LOSNum13 )
LOSNum13 <- as.numeric(LOSNum13)



LOSNum13 <- as.data.frame(LOSNum13)
summary(LOSNum13)

dat13$LOSNum13=LOSNum13$LOSNum
sd(dat13$LOSNum13, na.rm= TRUE)
var(dat13$LOSNum13, na.rm= TRUE)



# Outlires For Each Variable


#Ploting 2 Variables
## Plot pay of bush and obama 
#LOS-Pay
med_pay_by_LOS13 <- aggregate(Pay~LOSNum13, data = dat13, median)
summary(med_pay_by_LOS13)
LOSCor13<- dat13$LOSNum13
PayCor13<- dat13$Pay
cor.test(HHS_2013$Pay,LOSNum13)
cor.test(HHS_2013$Pay, HHS_2013$Education)
cor.test(HHS_2005$Pay, AgeNum05)

summary(x_2013)
#Age-PAy
med_pay_by_Age13 <- aggregate(Pay~AgeNum13, data = dat13, median)
summary(med_pay_by_Age13)
AgeCor13<- dat13$AgeNum13
cor.test(PayCor13,AgeCor13)

## visualize the distribution of employees in continental US ##
# adapted from:
# https://stackoverflow.com/questions/24441775/how-do-you-create-a-us-states-heatmap-based-on-some-values
# https://www.r-bloggers.com/us-state-maps-using-map_data/
library('ggplot2')
library('maps')
library('gridExtra')
state_trans <- read.csv("yarab.csv", header = TRUE)
write.csv(state_trans, file = "yarab.csv")
states <- map_data('state')
# prepend zero to states with number < 10
state_trans$Num <- sapply(state_trans$Num, FUN = function(x) formatC(x, width = 2, format = "d", flag = "0"))

# prep 2001
# get employment count per state
df_lower_states_2001 <- data.frame(table(tolower(state_trans$State[
  match(sapply(HHS_2001$Station, FUN = function(x) substring(x, 1,2)),
        state_trans$Num)])))

# create region column for matching
df_lower_states_2001$region <- df_lower_states_2001$Var1
df_lower_states_2001$Var1 <- NULL
# merge with states geospatial data
df_states_2001 <- merge(states, df_lower_states_2001, by = 'region', all.x = TRUE)

# prep 2013
# get employment count per state
df_lower_states_2013 <- data.frame(table(tolower(state_trans$State[
  match(sapply(HHS_2013$Station, FUN = function(x) substring(x, 1,2)),
        state_trans$Num)])))
# create region column for matching
df_lower_states_2013$region <- df_lower_states_2013$Var1
df_lower_states_2013$Var1 <- NULL
# merge with states geospatial data
df_states_2013 <- merge(states, df_lower_states_2013, by = 'region', all.x = TRUE)

# plot 2001
map_2001 <- ggplot() +
  geom_polygon(data = df_states_2001, aes(x = df_states_2001$long, y = df_states_2001$lat,
                                          group = df_states_2001$group,
                                          fill = df_states_2001$Freq),
               colour="white") +
  scale_fill_gradientn(colours = c("thistle2", "darkred"), limits = range(0,1000)) +
  theme_bw() + labs(fill = "Employees",
                    title = "HHS Employment, 2001", x="", y="") +
  scale_y_continuous(breaks = c()) + scale_x_continuous(breaks = c()) +
  theme(panel.border = element_blank(), plot.title = element_text(hjust = 0.5))

# plot 2013
map_2013 <- ggplot() +
  geom_polygon(data = df_states_2013, aes(x = df_states_2013$long, y = df_states_2013$lat,
                                          group = df_states_2013$group,
                                          fill = df_states_2013$Freq),
               colour="white") +
  scale_fill_gradientn(colours = c("thistle2", "darkred"), limits = range(0,1000)) +
  theme_bw() + labs(fill = "Employees",
                    title = "HHS Employment, 2013", x="", y="") +
  scale_y_continuous(breaks = c()) + scale_x_continuous(breaks = c()) +
  theme(panel.border = element_blank(), plot.title = element_text(hjust = 0.5))

grid.arrange(map_2001, map_2013, ncol = 2)
