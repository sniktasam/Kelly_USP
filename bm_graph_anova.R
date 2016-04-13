require(ggplot2)

x <- read.csv("~/Desktop/Revised_Kjorlien_USP.csv")
x <- data.frame(x)
x$kgha <- as.numeric(gsub(",", "", x$kgha))
df <- data.frame(x[c("Treatment", "kgha", "weedskgha")])
df$kgha <- as.numeric(gsub(",", "", df$kgha)) #change factors to numbers
df$weedskgha <- as.numeric(gsub(",", "", df$weedskgha)) #change factors to numbers

###create new df (df3) with special organization for plot3###
names(df)[3]<-"type"
df2 <- data.frame(df$Treatment, df$type)
names(df2)[1] <- "Treatment"
names(df2)[2] <- "kgha"
df2[2] <- df[3]
#df2
df2$type <- c("weedBM", "weedBM", "weedBM", "weedBM", "weedBM", "weedBM")
df$type <- c("totalBM", "totalBM", "totalBM", "totalBM", "totalBM", "totalBM")
names(df) == names(df2)
df3 <- rbind(df, df2)
df3
###

#plot total biomass with error bars
p1 <- ggplot(data = x, aes(x=Treatment, y=kgha))
p1 + 
  stat_summary(fun.y = mean, geom = "bar") + 
  stat_summary(fun.data = mean_se, geom = "errorbar", color = "red")

#plot weed biomass with standard error bars **need to remake df2 for this to work
#p2 <- ggplot(data = df2, aes(x=Treatment, y=weedskgha))
#p2 + 
#  stat_summary(fun.y = mean, geom = "bar") + 
#  stat_summary(fun.data = mean_se, geom = "errorbar", color = "red")

#plot total biomass and weed biomass on one graph with error bars 
p3 <- ggplot(data = df3, aes(Treatment, kgha, fill=type))
p3 + 
  stat_summary(fun.y = mean, position=position_dodge(1), geom = "bar") + 
  stat_summary(fun.data = mean_se, position=position_dodge(1), geom = "errorbar", color = "black", width=0.5) +
  theme_classic() +
  labs(title="Biomass by Treatment", x="Treatment", y="Kilograms per Hectare (kg/ha)") +
  scale_fill_discrete(name="Type",
                      breaks=c("totalBM", "weedBM"),
                      labels=c("Total Biomass", "Weed Biomass"))

#two sample t-test for difference of means between P/WP for soil nitrate
t.test(x$nitrate[x$Treatment == "P"],x$nitrate[x$Treatment =="WP"],alternative="two.sided")

#Create a LM for Nitrate by Treatment, w/Block enclosure
x$nitrate <- as.numeric(gsub(",", "", x$nitrate))
model <- lm(x$nitrate ~ x$Treatment + x$Block)
anova(model)
