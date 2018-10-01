install.packages("ggplot2")
install.packages("dplyr")
#.git loves me again
egg_data <- read.csv("data/egg_data.csv")
egg_data2018 <- read.csv("data/egg_data2018.csv")
egg_data2018b <- read.csv("data/egg_data2018b.csv")
library(ggplot2)
library(dplyr)
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

#2018

length_count8A <- lm(Count ~ Fish.Length+Age, data = egg_data2018, subset=Count !=351)
summary(length_count8A)
plot(length_count8A)
#2

length_count8B <- lm(Count ~ Fish.Length*Age, data = egg_data2018, subset=Count !=351)
summary(length_count8B)
plot(length_count8B)
#4

length_count8bA <- lm(Count ~ Fish.Length+Age, data = egg_data2018b, subset=Count !=351)
summary(length_count8bA)
plot(length_count8bA)
#1

length_count8bAS <- lm(Count ~ Fish.Length+Age+Site, data = egg_data2018b, subset=Count !=351)
summary(length_count8bA)
plot(length_count8bA)

length_count8EA <- lm(Count ~ Fish.Length+Eye, data = egg_data2018b, subset=Count !=351)
summary(length_count8EA)
plot(length_count8EB)

length_count8bB <- lm(Count ~ Fish.Length*Age, data = egg_data2018b, subset=Count !=351)
summary(length_count8bB)
plot(length_count8bB)
#3

length_count8EB <- lm(Count ~ Fish.Length*Eye, data = egg_data2018b, subset=Count !=351)
summary(length_count8EB)

length_count8SA <- lm(Count ~ Fish.Length+Site, data = egg_data2018b, subset=Count !=351)
summary(length_count8SA)

length_count8SB <- lm(Count ~ Fish.Length*Site, data = egg_data2018b, subset=Count !=351)
summary(length_count8SB)

length_count8n <- lm(Count ~ Fish.Length, data=egg_data2018b, subset=Count !=351)
summary(length_count8n)
length_count8i <- lm(Count ~ 1, data=egg_data2018b, subset=Count !=351)
summary(length_count8i)

AIC(length_count8bA, length_count8bB, length_count8n, length_count8i, length_count8EA, length_count8EB, length_count8bAS, length_count8SA, length_count8SB)

ggplot(data=egg_data2018b) +
  geom_point(aes(x=Fish.Length, y=Count, color = Age))

ggplot(egg_data2018b, aes(x=Fish.Length, y=Count, color = Age)) + geom_point() + geom_smooth(method='lm', se=FALSE)

CountPlot18 <- ggplot(egg_data2018b) + 
  aes(x=Fish.Length, y=Count, color=Age, shape=Age) + geom_point(size = 15) + 
  geom_abline(intercept = -613.20, slope = 116.58, size = 4, color = "brown") + 
  geom_abline(intercept = -362.29, slope = 116.58, size = 4, color = "red") +
  scale_color_manual(values = c("brown","red")) + theme_bw()

print(CountPlot18 + Arestytheme + labs(title = 
                                       "Length of Female Clownfish vs. Fecundity by Egg Age", 
                                     x = "Length (cm)", y = "Egg Count", 
                                     colour = "Egg Age", shape = "Egg Age"))



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

#ideas: exclude outliers and look for relationship between fish 
#length and density. 
#Also look for relationship between length+area to compare
