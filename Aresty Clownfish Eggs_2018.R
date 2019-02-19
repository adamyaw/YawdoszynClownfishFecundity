#.git loves me again
#test3
egg_data <- read.csv("data/egg_data.csv")
egg_data2018 <- read.csv("data/egg_data2018.csv")
egg_data2018b <- read.csv("data/egg_data2018b.csv")
egg_data2018t <- read.csv("data/egg_data2018b.txt")
library(ggplot2)
library(dplyr)
library(formattable)
################# 2017 ################
mutate(egg_data, Density = Count/Area)
egg_data$Age <- ifelse(egg_data$Color == "Red", c("Young"), c("Old"))
red_eggs <- filter(egg_data, Color == "Red")
red_eggs
redlength_count <- lm(Count ~ Fish.Length, data = red_eggs)
summary(redlength_count)
brown_eggs <- filter(egg_data, Color == "Brown")
brown_eggs
brownlength_count <- lm(Count ~ Fish.Length, data = brown_eggs)
summary(brownlength_count)
good_eggs <- filter(egg_data, Count != 720)
#needed a lower bin count for the histogram to work
FLq
geom_histogram(mapping = NULL, data = egg_data, stat = "bin", position = "stack", binwidth = NULL, bins = NULL, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)
hist(egg_data$Fish.Length)
hist(egg_data$Count)
hist(egg_data$Area)
#lm for linnear model
#color as an ordinal variable?
length_count <- lm(Count ~ Fish.Length, data = egg_data)
summary(length_count)
plot(length_count)
length_area <- lm(Area ~ Fish.Length, data = egg_data)
summary(length_area)

length_colcount <- lm (Count ~ Fish.Length+Color, data = egg_data)
summary(length_colcount)

length_count2 <- lm(Count ~ Fish.Length+Age, data = egg_data, subset=Count != 720)
summary(length_count2)
plot(length_count2)
#yes
#input estimations of model into table to update the graph

length_count2b <- lm(Count ~ Fish.Length*Age, data = egg_data, subset=Count != 720)
summary(length_count2b)

AIC(length_count2, length_count2b)

length_count3 <- lm(Count ~ Fish.Length+Color, data = egg_data, subset=Count != 720)
summary(length_count3)
plot(length_count3)

ggplot(egg_data, aes(x=Fish.Length, y=Count, color = Color)) + geom_point() + geom_smooth(method='lm', se=FALSE) + scale_color_manual(values = c("brown", "red", "gray"))

ggplot(data=egg_data) +
  geom_point(aes(x=Fish.Length, y=Count, color = Color))

CountPlot <- ggplot(subset(egg_data, Count<700), 
                    aes(x=Fish.Length, y=Count, color=Age, shape=Age)) + geom_point(size = 15) + 
  geom_abline(intercept = -585.93, slope = 106.61, size = 4, color = "brown") + 
  geom_abline(intercept = -426.57, slope = 106.61, size = 4, color = "red") +
  scale_color_manual(values = c("brown", "red")) + theme_bw()

Arestytheme <- theme(plot.title = element_text(family="Helvetica", 
                                               face="bold", size=(80)), legend.title = 
                       element_text(family="Helvetica", face="bold", 
                                    size=(72)), legend.text = element_text(family="Helvetica", 
                                                                           size=(64)), axis.title= element_text(family="Helvetica", 
                                                                                                                size=(72)), axis.text= element_text(family="Helvetica", 
                                                                                                                                                    size=(56)), legend.key = element_rect(size = 3),
                     legend.key.size = unit(4.8, 'lines'))

print(CountPlot + Arestytheme + labs(title = 
                      "Length of Female Clownfish vs. Fecundity by Egg Age", 
                                     x = "Length (cm)", y = "Egg Count", 
                                     colour = "Egg Age", shape = "Egg Age"))

################################ 2018 ####################
egg_data2018f <- subset(egg_data2018b[(egg_data2018b$Count!=351)&(egg_data2018b$Count!=426)&(egg_data2018b$Count!=364)&(egg_data2018b$Count!=670)&(egg_data2018b$Count!=557)&(egg_data2018b$Count!=688),])

length_count8bA <- lm(Count ~ Fish.Length+Age, data = egg_data2018f)
summary(length_count8bA)
plot(length_count8bA)
#1

length_count8bAS <- lm(Count ~ Fish.Length+Age+Site, data = egg_data2018f)
summary(length_count8bAS)
plot(length_count8bAS)

length_count8EA <- lm(Count ~ Fish.Length+Eye, data = egg_data2018f)
summary(length_count8EA)
plot(length_count8EB)

length_count8bB <- lm(Count ~ Fish.Length*Age, data = egg_data2018f)
summary(length_count8bB)
plot(length_count8bB)
#3

length_count8EB <- lm(Count ~ Fish.Length*Eye, data = egg_data2018f)
summary(length_count8EB)

length_count8SA <- lm(Count ~ Fish.Length+Site, data = egg_data2018f)
summary(length_count8SA)

length_count8SB <- lm(Count ~ Fish.Length*Site, data = egg_data2018f)
summary(length_count8SB)

length_count8n <- lm(Count ~ Fish.Length, data = egg_data2018f)
summary(length_count8n)
length_count8i <- lm(Count ~ 1, data = egg_data2018f)
summary(length_count8i)

AIC(length_count8bA, length_count8bB, length_count8n, length_count8i, length_count8EA, length_count8EB, length_count8bAS, length_count8SA, length_count8SB)


############### log, me #########
length_count8ll <- lm(log(Count) ~ log(Fish.Length), data = egg_data2018f)
summary(length_count8ll)
length_count8lli <- lm(log(Count) ~ 1, data = egg_data2018f)
summary(length_count8lli)

length_count8llA <- lm(log(Count) ~ log(Fish.Length)+Age, data = egg_data2018f)
summary(length_count8llA)
plot(length_count8llA)

length_count8llAS <- lm(log(Count) ~ log(Fish.Length)+Age+Site, data = egg_data2018f)
summary(length_count8llAS)

length_count8llEA <- lm(log(Count) ~ log(Fish.Length)+Eye, data = egg_data2018f)
summary(length_count8llEA)
plot(length_count8llEA)

length_count8llB <- lm(log(Count) ~ log(Fish.Length)*Age, data = egg_data2018f)
summary(length_count8llB)

length_count8llEB <- lm(log(Count) ~ log(Fish.Length)*Eye, data = egg_data2018f)
summary(length_count8llEB)

length_count8llSA <- lm(log(Count) ~ log(Fish.Length)+Site, data = egg_data2018f)
summary(length_count8llSA)

length_count8llSB <- lm(log(Count) ~ log(Fish.Length)*Site, data = egg_data2018f)
summary(length_count8llSB)

AIC(length_count8llA, length_count8llB, length_count8ll, length_count8lli, length_count8llEA, length_count8llEB, length_count8llAS, length_count8llSA, length_count8llSB)

############### log Both, Nolan ################

length_count8llN <- lm(log(NolCount) ~ log(Fish.Length), data = egg_data2018f)
summary(length_count8llN)
length_count8llNi <- lm(log(NolCount) ~ 1, data = egg_data2018f)
summary(length_count8llNi)

length_count8llNA <- lm(log(NolCount) ~ log(Fish.Length)+Age, data = egg_data2018f)
summary(length_count8llNA)
plot(length_count8llNA)

length_count8llNAS <- lm(log(NolCount) ~ log(Fish.Length)+Age+Site, data = egg_data2018f)
summary(length_count8llNAS)

length_count8llNEA <- lm(log(NolCount) ~ log(Fish.Length)+Eye, data = egg_data2018f)
summary(length_count8llNEA)
plot(length_count8llNEA)

length_count8llNB <- lm(log(NolCount) ~ log(Fish.Length)*Age, data = egg_data2018f)
summary(length_count8llNB)

length_count8llNEB <- lm(log(NolCount) ~ log(Fish.Length)*Eye, data = egg_data2018f)
summary(length_count8llNEB)

length_count8llNSA <- lm(log(NolCount) ~ log(Fish.Length)+Site, data = egg_data2018f)
summary(length_count8llNSA)

length_count8llNSB <- lm(log(NolCount) ~ log(Fish.Length)*Site, data = egg_data2018f)
summary(length_count8llNSB)

AIC(length_count8llNA, length_count8llNB, length_count8llN, length_count8llNi, length_count8llNEA, length_count8llNEB, length_count8llNAS, length_count8llNSA, length_count8llNSB)


###################### log Count, Nolan ############

length_count8lN <- lm(log(NolCount) ~ Fish.Length, data = egg_data2018f)
summary(length_count8lN)
length_count8lNi <- lm(log(NolCount) ~ 1, data = egg_data2018f)
summary(length_count8lNi)

length_count8lNA <- lm(log(NolCount) ~ Fish.Length+Age, data = egg_data2018f)
summary(length_count8lNA)
plot(length_count8lNA)

length_count8lNAS <- lm(log(NolCount) ~ Fish.Length+Age+Site, data = egg_data2018f)
summary(length_count8lNAS)

length_count8lNEA <- lm(log(NolCount) ~ Fish.Length+Eye, data = egg_data2018f)
summary(length_count8lNEA)
plot(length_count8lNEA)

length_count8lNB <- lm(log(NolCount) ~ Fish.Length*Age, data = egg_data2018f)
summary(length_count8lNB)

length_count8lNEB <- lm(log(NolCount) ~ Fish.Length*Eye, data = egg_data2018f)
summary(length_count8lNEB)

length_count8lNSA <- lm(log(NolCount) ~ Fish.Length+Site, data = egg_data2018f)
summary(length_count8llSA)

length_count8lNSB <- lm(log(NolCount) ~ Fish.Length*Site, data = egg_data2018f)
summary(length_count8lNSB)

AIC(length_count8lNA, length_count8lNB, length_count8lN, length_count8lNi, length_count8lNEA, length_count8lNEB, length_count8lNAS, length_count8lNSA, length_count8lNSB)


#################### log transforming only count #########

length_count8lC <- lm(log(Count) ~ Fish.Length, data = egg_data2018f)
summary(length_count8lC)
length_count8lCi <- lm(log(Count) ~ 1, data = egg_data2018f)
summary(length_count8lCi)

length_count8lCA <- lm(log(Count) ~ Fish.Length+Age, data = egg_data2018f)
summary(length_count8lCA)
plot(length_count8lCA)

length_count8lCAS <- lm(log(Count) ~ Fish.Length+Age+Site, data = egg_data2018f)
summary(length_count8lCAS)

length_count8lCEA <- lm(log(Count) ~ Fish.Length+Eye, data = egg_data2018f)
summary(length_count8lCEA)
plot(length_count8lCEA)

length_count8lCB <- lm(log(Count) ~ Fish.Length*Age, data = egg_data2018f)
summary(length_count8lCB)

length_count8lCEB <- lm(log(Count) ~ Fish.Length*Eye, data = egg_data2018f)
summary(length_count8lCEB)

length_count8lCSA <- lm(log(Count) ~ Fish.Length+Site, data = egg_data2018f)
summary(length_count8lCSA)

length_count8lCSB <- lm(log(Count) ~ Fish.Length*Site, data = egg_data2018f)
summary(length_count8lCSB)

AIC(length_count8lCA, length_count8lCB, length_count8lC, length_count8lCi, length_count8lCEA, length_count8lCEB, length_count8lCAS, length_count8lCSA, length_count8lCSB)

length_count8lLEA <- lm(Count ~ log(Fish.Length)+Eye, data = egg_data2018f)
summary(length_count8lLEA)

################ plotting #############

ggplot(data=egg_data2018f) +
  geom_point(aes(x=Fish.Length, y=Count, color = Eye))

ggplot(egg_data2018b, aes(x=Fish.Length, y=Count, color = Age)) + geom_point() + geom_smooth(method='lm', se=FALSE)

#fully log transformed
CountPlot18 <- ggplot(egg_data2018f,
  aes(x=log(Fish.Length), y=log(Count), color=Eye, shape=Eye))+ geom_point(size = 15) + 
  geom_abline(intercept = .5655, slope = 2.3881, size = 4, color = "brown") + 
  geom_abline(intercept = 1.1738, slope = 2.3881, size = 4, color = "red") + 
  scale_color_manual(values = c("red","brown")) + theme_bw()

#log-scale axes (update, I cannot figure out the way to do this how I want)
CountPlotlog18 <- ggplot(egg_data2018f,
                      aes(x=log(Fish.Length), y=log(Count), color=Eye, shape=Eye))+ geom_point(size = 15) + 
  geom_abline(intercept = -761.74, slope = 127.99, size = 4, color = "brown") + 
  geom_abline(intercept = -457.81, slope = 127.99, size = 4, color = "red") + 
  scale_color_manual(values = c("red","brown")) + theme_bw() + scale_x_log10() + scale_y_log10()

CountPlotlog18 + annotation_logticks()
#that did not work

#how to get subscript?
print(CountPlot18 + Arestytheme + labs(title = 
                                       "Length of Female Clownfish vs. Fecundity by Egg Eyeing", 
                                     x = "logLength (cm)", y = "logEgg Count", 
                                     colour = "Egg Eyes", shape = "Egg Eyes"))

fishtab = read.table("data/egg_data2018b.txt", header=T, sep='\t', stringsAsFactors = F)
Clownfish18tab = fishtab[c(1,3,5,6,7,8,9,10)]
widget_formattable = formattable(Clownfish18tab)
widget_formattable



ggplot(egg_data, aes(x=Fish.Length, y=Area, color = Color)) +
  geom_point() + geom_smooth(method='lm', se=FALSE)
length_area <- lm(Area ~ Fish.Length, data = egg_data) 
summary(length_area)
plot(length_area)

ggplot(egg_data, aes(x=Area, y=Count, color = Color)) +
  geom_point()

Density <- (egg_data$Count / egg_data$Area)
summary(Density)
ggplot(egg_data, aes(x=Fish.Length, y=Density, color = Color)) + 
  geom_point() + geom_smooth(method='lm', se=FALSE) +
  scale_color_manual(values = c("brown", "red", "gray"))

length_density <- lm(Fish.Length~Density, data = egg_data)
summary(length_density)
plot(length_density)
length_density2 <- lm(Fish.Length~Density, data = egg_data, subset = Density <78)
summary(length_density2)
plot(length_density2)
length_density3 <- lm(Fish.Length~Density+Color, data = egg_data)
summary(length_density3)
plot(length_density3)

ggplot(egg_data, aes(x=Area, y=Density, color = Color)) + 
  geom_point() + geom_smooth(method='lm', se=FALSE) + 
  scale_color_manual(values = c("brown", "red", "gray"))

area_density <- lm(Area~Density, data = egg_data)
summary(area_density)
plot(area_density)

area_density2 <- lm(Area~Density+Color, data = egg_data)
summary(area_density2)
plot(area_density2)

sqrt(var(egg_data2018f$Count))
#ideas: exclude outliers and look for relationship between fish 
#length and density. 
#Also look for relationship between length+area to compare
