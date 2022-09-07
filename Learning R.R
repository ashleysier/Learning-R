### Code used in tutorials to learn R

#Data Science: R Basics

library(dslabs)
library(tidyverse)
# library(tidyverse)
        
data(murders)

murders %>%
  ggplot(aes(population,total,label=abb,color=region)) + 
  geom_label()

a <- 1
b <- 1
c <- -1

print(a)

ls()

solution_1 <- (-b + sqrt(b^2 - 4*a*c))/(2*a)
solution_2 <- (-b - sqrt(b^2 - 4*a*c))/(2*a)

log(8)
log(a)
exp(1)
log(2.718)
log(exp(1))
help("log")
args(log)
log(8,base=2)
log(x=8,base=2)
log(8,2)
2^3
help("+")
?"+"
pi
Inf

x <- 3
solution_3 <- (3*x^2 + 2*x -1)

class(ls)

library(dslabs)
data("murders")
class(murders)
str(murders)
head(murders)
names(murders)

murders$population

pop <- murders$population
length(pop)

a <- 1
a
"a"
 class(murders$state)
 
z <- 3==2
z
class(z)

class(murders$region)
levels(murders$region)

# Assessment 1
a <- 2
b <- -1
c <- -4
solution_1
solution_2

log(1024,4)

data(movielens)
str(movielens)

class(movielens$title)

class(movielens$genres)

nlevels(movielens$genres)


# Assessment #2
x <- c(2,43,27,96,18)
sort(x)
order(x)
rank(x)

min(x)
which.min(x)
max(x)
which.max(x)

name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)

time_hr <-time/60
speed <-distance/time_hr

data.frame(name,time_hr)
data.frame(name,speed)

# Assessment #3
data(heights)
options(digits=3)
str(heights)

mean <-mean(heights$height)
ind <- heights$height>mean
table(ind)

table(ind,heights$sex)
help(table)
238/1050


min <- min(heights$height)
match <- match(min,heights$height)

data.frame(sex=heights$sex[1032])

max <- max(heights$height)
max

integer(max)
x<- 50:82

sum(!x %in% heights$height)

heights$ht_cm <- heights$height*2.54
heights2 <- heights

data.frame(height=heights2$ht_cm[18])
mean <- mean(heights2$ht_cm)
mean

class(heights2$sex)
levels(heights2$sex)
females <- filter(heights2,sex=="Female")

mean <- mean(females$ht_cm)
mean

data(olive)
head(olive)

plot(olive$palmitic,olive$palmitoleic)

hist(olive$eicosenoic)

boxplot(palmitic~region,data=olive)
levels(olive$region)

codes <- c(Italy=380, Canda=124, Egypt=818)
codes
codes <- c(380,124,818)
country <- c("Italy","Canada","Egypt")
names(codes) <- country
codes

codes[2]
codes[c(1,3)]
codes[1:3]
codes[c("Canada","Egypt")]

x <- c(1,"canada",3)
x

sort(murders$total)
index <- order(murders$total)
murders$total[index]
murders$abb[index]

max <- murders$total
max
i_max <- which.max(murders$total)
i_max
murders$state[i_max]

murders$state[which.max(murders$population)]

my_quantile <- function(x){
    r <- quantile(x, c(0,0.5,1))
    data.frame(minimum=r[1],median=r[2],maximum=r[3])
}

murders %>% filter(region=="West") %>% summarize(my_quantile(total/population*100000))

us_murder_rate <- murders %>% summarize(rate=sum(total)/sum(population)*10^5) %>% pull(rate)
us_murder_rate

us_murder_rate <- murders %>% summarize(rate=sum(total)/sum(population)*10^5) %>% .$rate
us_murder_rate

murders %>% group_by(region) %>% summarize(median=median(total/population*100000))

murders %>% arrange(population) %>% head()

library(data.table)
murders <- setDT(murders)
murders[, rate := total/population * 100000]
murders[, ":="(rate = total/population * 100000, rank = rank(population))]

y <- copy(x)

murders[rate <= 0.7, .(state, rate)]

heights <- setDT(heights)
s <- heights[, .(average=mean(height), standard_deviation=sd(height))]
median_min_max <- function(x){
  qs <- quantie(x, c(0.5,0,1))
  data.frame(median=qs[1], median=qs[2], maximum=qs[3])
}

heights[,.(average=mean(height), standard_deviation=sd(height)),by=sex]

murders[order(population)] |> head()
murders[order(population, decreasing=TRUE)] |> head()
murders[order(region,rate)]

murder_rate <- murders$total/murders$population*100000
ind <- which.min(murder_rate)
if(murder_rate[ind] < 0.5){
  print(murders$state[ind])
} else {
  print("No state has murder rate that low")
}

a <- c(0,1,2,-4,5)
ifelse(a>0 , 1/a, NA)

for(i in 1:5){
  print(i)
}


# Data Science: Data Visualization
#Creating a CDF plot
a <- seq(min(my_data), max(my_data), length = 100)    # define range of values spanning the dataset
cdf_function <- function(x) {    # computes prob. for a single value
  mean(my_data <= x)
}
cdf_values <- sapply(a, cdf_function)
plot(a, cdf_values)

# z score
z <- scale(x)

# p
mean(abs(z) < 2)

# taller than certain height
1 - pnorm(70.5, mean(x), sd(x))

#quantiles
library(dslabs)
data(heights)

summary(heights$height)

p <- seq(0.01, 0.99, 0.01)
percentiles <- quantile(heights$height, p)

percentiles[names(percentiles) == "25%"]
percentiles[names(percentiles) == "75%"]

#
qnorm(p, mu, sigma)

#By default, mu=0 and sigma=1. Therefore, calling qnorm() with no arguments gives quantiles for the standard normal distribution.
qnorm(p)

p <- seq(0.01, 0.99, 0.01)
theoretical_quantiles <- qnorm(p, 69, 3)

plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

#Plotting graphs using ggplot2
library(tidyverse)
library(dslabs)
data(murders)

ggplot(data=murders)
p <- ggplot(data=murders)
class(p)
p

murders %>% ggplot() + geom_point(aes(x=population/10^6,y=total),size=3) +geom_text(aes(population/10^6,total, label=abb), nudge_x=1)
p <- murders %>% ggplot(aes(population/10^6,total,label=abb))
p + geom_point(size=3) + geom_text(nudge_x=0.05) + scale_x_continuous(trans="log10") + scale_y_continuous(trans="log10")
p + geom_point(size=3) + geom_text(nudge_x=0.05) + scale_x_log10() + scale_y_log10() + 
  xlab("Populations in millions (log scale)") + ylab("Total number of murders (log scale)") + ggtitle("US Gun Muders in US 2010")

ds_theme_set()
install.packages("ggthemes")
library(ggthemes)

install.packages("ggrepel")
library(ggrepel)

install.packages("gridExtra")
library(gridExtra)


p <- murders %>% ggplot(aes(population/10^6,total,label=abb)) + scale_x_log10() + scale_y_log10() + 
  xlab("Populations in millions (log scale)") + ylab("Total number of murders (log scale)") + ggtitle("US Gun Muders in US 2010")
p + theme_economist() + geom_abline(intercept = log10(r),lty = 2, color = "darkgrey") + geom_point(aes(col=region),size=3) + geom_text_repel() + scale_color_discrete(name = "Region") 
r <- murders %>% summarize(rate=sum(total)/sum(population)*10^6) %>% .$rate


p <- heights %>% filter(sex=="Male") %>% ggplot(aes(x=height))
p + geom_histogram(binwidth=1, fill="blue", col="black") + xlab("Male heights in inches") + ggtitle("Histogram")
p + geom_density(fill="blue")


p <- heights %>% filter(sex=="Male") %>% ggplot(aes(sample=height))
p + geom_qq()
params <- heights %>% filter(sex == "Male") %>% summarize(mean = mean(height), sd = sd(height))
p + geom_qq(dparams = params) + geom_abline()

heights %>% ggplot(aes(sample=scale(height))) + geom_qq() + geom_abline()

p <- heights %>% filter(sex == "Male") %>% ggplot(aes(x = height))
p1 <- p + geom_histogram(binwidth = 1, fill = "blue", col = "black")
p2 <- p + geom_histogram(binwidth = 2, fill = "blue", col = "black")
p3 <- p + geom_histogram(binwidth = 3, fill = "blue", col = "black")
grid.arrange(p1,p2,p3, ncol=3)

data(gapminder)
head(gapminder)

gapminder %>% filter(year == 2015 & country %in% c("Sri Lanka", "Turkey")) %>% select(country, infant_mortality)

ds_theme_set()
filter(gapminder, year %in%c(1962,2012)) %>% ggplot(aes(fertility, life_expectancy, color=continent)) + geom_point() + facet_grid((.~year))

years <- c(1962, 1980, 1990, 2000, 2012)
continents <- c("Europe", "Asia")
gapminder %>% filter(year %in% years & continent %in% continents) %>% ggplot(aes(fertility, life_expectancy, col=continent)) + geom_point() + facet_wrap(~year)
gapminder %>% filter(country %in% c("South Korea","Germany")) %>% ggplot(aes(year, fertility,col=country)) + geom_line()

countries <- c("South Korea", "Germany")
labels <- data.frame(country = countries, x = c(1975, 1965), y = c(60, 72))
gapminder %>% filter(country %in% countries) %>% ggplot(aes(year, life_expectancy, col = country)) + geom_line() + geom_text(data = labels, aes(x, y, label = country), size = 5) + theme(legend.position = "none")

gapminder <- gapminder %>% mutate(dollars_per_day=gdp/population/365)
gapminder %>% filter(year==1970 & !is.na(gdp)) %>% ggplot(aes(log2(dollars_per_day))) + geom_histogram(binwidth=1, col="black")
gapminder %>% filter(year==1970 & !is.na(gdp)) %>% ggplot(aes(dollars_per_day)) + geom_histogram(binwidth=1, col="black") + scale_x_continuous(trans="log2")

p <- gapminder %>% filter(year==1970 & !is.na(gdp)) %>% ggplot(aes(region,dollars_per_day))
p + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

p <- gapminder %>% filter(year==1970 & !is.na(gdp)) %>% mutate(region=reorder(region,dollars_per_day, FUN=median)) %>% ggplot(aes(region,dollars_per_day,fill=continent)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p + scale_y_continuous(trans="log2") + geom_point(show.legend=FALSE)

west <- c("Western Europe", "Northern Europe", "Southern Europe")
gapminder %>% filter(year == 1970 & !is.na(gdp)) %>%mutate(group = ifelse(region %in% west, "West", "Developing")) %>% ggplot(aes(dollars_per_day)) + geom_histogram(binwidth = 1, color = "black") + scale_x_continuous(trans = "log2") + facet_grid(. ~ group)
gapminder %>% filter(year %in% c(1970,2010) & !is.na(gdp)) %>%mutate(group = ifelse(region %in% west, "West", "Developing")) %>% ggplot(aes(dollars_per_day)) + geom_histogram(binwidth = 1, color = "black") + scale_x_continuous(trans = "log2") + facet_grid(year ~ group)
country_list_1 <- gapminder %>% filter(year==1970 & !is.na(dollars_per_day)) %>% .$country
country_list_2 <- gapminder %>% filter(year==2010 & !is.na(dollars_per_day)) %>% .$country
country_list <- intersect(country_list_1,country_list_2)
gapminder %>% filter(year %in% c(1970,2010) & !is.na(gdp) & country %in% country_list) %>%mutate(group = ifelse(region %in% west, "West", "Developing")) %>% ggplot(aes(dollars_per_day)) + geom_histogram(binwidth = 1, color = "black") + scale_x_continuous(trans = "log2") + facet_grid(year ~ group)

p <- gapminder %>% filter(year %in% c(1970,2010) & !is.na(gdp)) %>% mutate(region=reorder(region,dollars_per_day, FUN=median)) %>% ggplot(aes(region,dollars_per_day,fill=continent)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab(" ") + scale_y_continuous("log2")
p + geom_boxplot(aes(region,dollars_per_day,fill=factor(year)))

p <- gapminder %>% filter(year %in% c(1970,2010) & country %in% country_list) %>% mutate(group=ifelse(region %in% west, "West", "Developing")) %>% ggplot(aes(dollars_per_day, y=..count..,fill=group)) + scale_x_continuous("log2")
p + geom_density(alpha=0.2, bw=0.75) + facet_grid(year~.)

gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "West",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    TRUE ~ "Others"))

gapminder <- gapminder %>%
  mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))

p <- gapminder %>%
  filter(year %in% c(1970,2010) & country %in% country_list) %>%
  ggplot(aes(dollars_per_day, fill = group)) +
  scale_x_continuous(trans = "log2")

# stacked density plot
p + geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
  facet_grid(year ~ .)

gapminder %>%
  filter(year %in% c(1970, 2010) & country %in% country_list) %>%
  group_by(year) %>%
  mutate(weight = population/sum(population*2)) %>%
  ungroup() %>%
  ggplot(aes(dollars_per_day, fill = group, weight = weight)) +
  scale_x_continuous(trans = "log2") +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") + facet_grid(year ~ .)

gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "The West",
    .$region %in% "Northern Africa" ~ "Northern Africa",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region == "Southern Asia" ~ "Southern Asia",
    .$region %in% c("Central America", "South America", "Caribbean") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"))

surv_income <- gapminder %>%
  filter(year %in% 2010 & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
  group_by(group) %>%
  summarize(income = sum(gdp)/sum(population)/365,
            infant_survival_rate = 1 - sum(infant_mortality/1000*population)/sum(population))
surv_income %>% arrange(income)

surv_income %>% ggplot(aes(income, infant_survival_rate, label = group, color = group)) +
  scale_x_continuous(trans = "log2", limit = c(0.25, 150)) +
  scale_y_continuous(trans = "logit", limit = c(0.875, .9981),
                     breaks = c(.85, .90, .95, .99, .995, .998)) +
  geom_label(size = 3, show.legend = FALSE) 

#slope chart
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

dat <- gapminder %>%
  filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7)

dat %>%
  mutate(location = ifelse(year == 2010, 1, 2),
         location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"),
                           location + 0.22, location),
         hjust = ifelse(year == 2010, 1, 0)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(year, life_expectancy, group = country)) +
  geom_line(aes(color = country), show.legend = FALSE) +
  geom_text(aes(x = location, label = country, hjust = hjust), show.legend = FALSE) +
  xlab("") +
  ylab("Life Expectancy") 

#Band-Altmna Graph
library(ggrepel)
dat %>%
  mutate(year = paste0("life_expectancy_", year)) %>%
  select(country, year, life_expectancy) %>% spread(year, life_expectancy) %>%
  mutate(average = (life_expectancy_2015 + life_expectancy_2010)/2,
         difference = life_expectancy_2015 - life_expectancy_2010) %>%
  ggplot(aes(average, difference, label = country)) +
  geom_point() +
  geom_text_repel() +
  geom_abline(lty = 2) +
  xlab("Average of 2010 and 2015") +
  ylab("Difference between 2015 and 2010")

#tile plot
data(us_contagious_diseases)
str(us_contagious_diseases)
the_disease <- "Measles"
dat <- us_contagious_diseases %>%
  filter(!state %in% c("Hawaii", "Alaska") & disease == the_disease) %>%
  mutate(rate = count / population * 10000 * 52/weeks_reporting) %>%
  mutate(state = reorder(state, rate))

dat %>% filter(state == "California" & !is.na(rate)) %>%
  ggplot(aes(year, rate)) +
  geom_line() +
  ylab("Cases per 10,000") +
  geom_vline(xintercept=1963, col = "blue")

install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all(type="seq")
display.brewer.all(type="div")

dat %>% ggplot(aes(year, state, fill=rate)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt") +
  geom_vline(xintercept = 1963, col = "blue") +
  theme_minimal() + theme(panel.grid = element_blank()) +
  ggtitle(the_disease) +
  ylab("") +
  xlab("")

avg <- us_contagious_diseases %>%
  filter(disease == the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm = TRUE)/sum(population, na.rm = TRUE)*10000)

dat %>%
  filter(!is.na(rate)) %>%
  ggplot() +
  geom_line(aes(year, rate, group = state), color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate), data = avg, size = 1, col = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) +
  ggtitle("Cases per 10,000 by state") +
  xlab("") +
  ylab("") +
  geom_text(data = data.frame(x = 1955, y = 50),
            mapping = aes(x, y, label = "US average"), color = "black") +
  geom_vline(xintercept = 1963, col = "blue")

#sig figs
options(digits=2)

#Titanic Assignment
install.packages("titanic")

options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

?titanic_train
head(PClass)

titanic %>% filter(!is.na(Age)) %>% group_by(Sex) %>% ggplot(aes(Age)) + geom_density(position = "stack") + facet_grid(Sex ~ .)

titanic %>% filter(!is.na(Age)) %>% group_by(Sex) %>% ggplot(aes(Age, y=..count..)) + geom_density() + facet_grid(Sex~.)

titanic %>% filter(!is.na(Age)) %>% group_by(Sex) %>% ggplot(aes(Age, y=..count..)) + geom_density() + facet_grid(Sex~.)

titanic %>% filter(!is.na(Age)) %>% group_by(Sex) %>% ggplot(aes(Age,fill=Sex)) + geom_density(position = "stack", alpha=0.2) + facet_grid(Sex ~ .)

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

titanic %>% filter(!is.na(Age)) %>% ggplot(aes(sample=Age)) + geom_qq(dparams=params) + geom_abline()

titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Survived, fill=Sex)) + geom_bar(position = position_dodge())
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Sex, fill=Survived)) + geom_bar(position = position_dodge())

titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age, fill=Survived)) + geom_density(alpha=0.2)
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age, fill=Survived,y=..count..)) + geom_density(alpha=0.2)

titanic %>% filter(Fare!=0) %>% ggplot(aes(Fare,Survived)) + geom_boxplot()  + scale_x_continuous(trans="log2") + geom_point() + geom_jitter(alpha = 0.2)

titanic %>% ggplot(aes(Pclass,fill=Survived)) + geom_bar()
titanic %>% ggplot(aes(Pclass,fill=Survived)) + geom_bar(position = position_fill())
titanic %>% ggplot(aes(Survived,fill=Pclass)) + geom_bar(position = position_fill())

titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age, fill=Survived,y=..count..)) + geom_density(alpha=0.2) + facet_grid(Sex~Pclass)


# Probability Course
beads <- rep(c("red","blue"),times=c(2,3))
beads
sample(beads,1)
sample(beads,5)


B <- 10000    # number of times to draw 1 bead
events <- replicate(B, sample(beads, 1))    # draw 1 bead, B times
tab <- table(events)    # make a table of outcome counts
tab    # view count table
prop.table(tab)    # view table of outcome proportions

events <- replicate(B, sample(beads, 1),replace=TRUE)    # draw 1 bead, B times
tab <- table(events)    # make a table of outcome counts
tab    # view count table
prop.table(tab)    # view table of outcome proportions

set.seed(1986)
set.seed(1,sample.kind="Rounding")


# joining strings with paste
number <- "Three"
suit <- "Hearts"
paste(number, suit)

# joining vectors element-wise with paste
paste(letters[1:5], as.character(1:5))

# generating combinations of 2 vectors with expand.grid
expand.grid(pants = c("blue", "black"), shirt = c("white", "grey", "plaid"))

# generate deck of cards
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)

# probability of drawing a king
kings <- paste("King", suits)
mean(deck %in% kings)

install.packages("gtools")
library(gtools)
permutations(5,2)    # ways to choose 2 numbers in order from 1:5
all_phone_numbers <- permutations(10, 7, v = 0:9)
n <- nrow(all_phone_numbers)
index <- sample(n, 5)
all_phone_numbers[index,]
permutations(3,2)    # order matters
combinations(3,2)    # order does not matter

hands <- permutations(52,2, v = deck)
first_card <- hands[,1]
second_card <- hands[,2]
sum(first_card %in% kings)

sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)

aces <- paste("Ace", suits)
facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)

hands <- combinations(52, 2, v=deck) # all possible hands

# probability of a natural 21 given that the ace is listed first in `combinations`
mean(hands[,1] %in% aces & hands[,2] %in% facecard)

# probability of a natural 21 checking for both ace first and ace second
mean((hands[,1] %in% aces & hands[,2] %in% facecard)|(hands[,2] %in% aces & hands[,1] %in% facecard))

# code for one hand of blackjack
hand <- sample(deck, 2)
hand

# code for B=10,000 hands of blackjack
B <- 10000
results <- replicate(B, {
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) | (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)

# checking for duplicated bdays in one 50 person group
n <- 50
bdays <- sample(1:365, n, replace = TRUE)    # generate n random birthdays
any(duplicated(bdays))    # check if any birthdays are duplicated

# Monte Carlo simulation with B=10000 replicates
B <- 10000
results <- replicate(B, {    # returns vector of B logical values
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
})
mean(results)    # calculates proportion of groups with duplicated bdays


# function to calculate probability of shared bdays across n people
compute_prob <- function(n, B = 10000) {
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

n <- seq(1, 60)

x <- 1:10
sqrt(x)    # sqrt operates on each element of the vector

y <- 1:10
x*y    # * operates element-wise on both vectors

compute_prob(n)    # does not iterate over the vector n without sapply

x <- 1:10
sapply(x, sqrt)    # this is equivalent to sqrt(x)

prob <- sapply(n, compute_prob)    # element-wise application of compute_prob to n
plot(n, prob)

# function for computing exact probability of shared birthdays for any n
exact_prob <- function(n){
  prob_unique <- seq(365, 365-n+1)/365   # vector of fractions for mult. rule
  1 - prod(prob_unique)    # calculate prob of no shared birthdays and subtract from 1
}

# applying function element-wise to vector of n values
eprob <- sapply(n, exact_prob)

# plotting Monte Carlo results and exact probabilities on same graph
plot(n, prob)    # plot Monte Carlo results
lines(n, eprob, col = "red")    # add line for exact prob

B <- 10^seq(1, 5, len = 100)    # defines vector of many B values
compute_prob <- function(B, n = 22){    # function to run Monte Carlo simulation with each B
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

prob <- sapply(B, compute_prob)    # apply compute_prob to many values of B
plot(log10(B), prob, type = "l")    # plot a line graph of estimates 

# Monty Hall Problem
B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)    # open door with no prize that isn't chosen
  stick <- my_pick    # stick with original door
  stick == prize_door    # test whether the original door has the prize
})
mean(stick)    # probability of choosing prize door when sticking

switch <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen first
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)    # open door with no prize that isn't chosen
  switch <- doors[!doors%in%c(my_pick, show)]    # switch to the door that wasn't chosen first or opened
  switch == prize_door    # test whether the switched door has the prize
})
mean(switch)    # probability of choosing prize door when switching



# Evaluation
permutations(8,3)
permutations(3,3)
(3/8)*(2/7)*(1/6)

runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
set.seed(1,sample.kind="Rounding")

B <- 10000
results <- replicate(B, {    # returns vector of B logical values
  winners <- sample(runners, 3)
  #all(winners, na.rm=TRUE)=="Jamaica"
})

results <- replicate(10000, {
  winners <- sample(runners, 3)
  all(winners=="Jamaica")
  })
mean(results)

combinations(6,2,repeats.allowed=FALSE)
15*6*2
15*6*3
dim(combinations(6,3,repeats.allowed=FALSE))[1] * 6*3
20*6*3

dinner <- function(x) {
  result <- x * 15 * 3
}
sapply(1:12,dinner)

dinner <- function(x) {
  result <- dim(combinations(x,2,repeats.allowed=FALSE))[1] * 6*3
}

sapply(2:12,dinner)


data(esoph)
all_cases=sum(esoph$ncases)
all_cases
all_controls=sum(esoph$ncontrols)
all_controls
esoph %>% filter(alcgp=="120+") %>% summarize(sum(ncases))/ (esoph %>% filter(alcgp=="120+") %>% summarize(sum(ncases)) + esoph %>% filter(alcgp=="120+") %>% summarize(sum(ncontrols)))
esoph %>% filter(alcgp=="0-39g/day") %>% summarize(sum(ncases))/ (esoph %>% filter(alcgp=="0-39g/day") %>% summarize(sum(ncases)) + esoph %>% filter(alcgp=="0-39g/day") %>% summarize(sum(ncontrols)))
esoph %>% filter(tobgp!="0-9g/day") %>% summarize(sum(ncases))/all_cases
esoph %>% filter(tobgp!="0-9g/day") %>% summarize(sum(ncontrols))/all_controls
esoph %>% filter(alcgp=="120+") %>% summarize(sum(ncases))/all_cases
esoph %>% filter(tobgp=="30+") %>% summarize(sum(ncases))/all_cases
(esoph %>% filter(alcgp=="120+" & tobgp!="30+") %>% summarize(sum(ncases)) + esoph %>% filter(alcgp!="120+" & tobgp=="30+") + esoph %>% filter(alcgp=="120+" & tobgp=="30+") %>% summarize(sum(ncases))) 
(esoph %>% filter(alcgp=="120+" & tobgp!="30+") %>% summarize(sum(ncases)) + esoph %>% filter(alcgp!="120+" & tobgp=="30+") %>% summarize(sum(ncases)) + esoph %>% filter(alcgp=="120+" & tobgp=="30+") %>% summarize(sum(ncases)))/all_cases
esoph %>% filter(alcgp=="120+") %>% summarize(sum(ncontrols))/all_controls   
(esoph %>% filter(alcgp=="120+") %>% summarize(sum(ncases))/all_cases) / (esoph %>% filter(alcgp=="120+") %>% summarize(sum(ncontrols))/all_controls) 
esoph %>% filter(tobgp=="30+") %>% summarize(sum(ncontrols))/all_controls
esoph %>% filter(tobgp=="30+" & alcgp=="120+") %>% summarize(sum(ncontrols))/all_controls
((esoph %>% filter(alcgp=="120+" & tobgp!="30+") %>% summarize(sum(ncases)) + esoph %>% filter(alcgp!="120+" & tobgp=="30+") %>% summarize(sum(ncases)) + esoph %>% filter(alcgp=="120+" & tobgp=="30+") %>% summarize(sum(ncases)))/all_cases)/((esoph %>% filter(alcgp=="120+" & tobgp!="30+") %>% summarize(sum(ncontrols)) + esoph %>% filter(alcgp!="120+" & tobgp=="30+") %>% summarize(sum(ncontrols)) + esoph %>% filter(alcgp=="120+" & tobgp=="30+") %>% summarize(sum(ncontrols)))/all_controls)


#Continuous probability
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

F <- function(a) mean(x <= a)
1 - F(70)    # probability of male taller than 70 inches

library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

1 - pnorm(70.5, mean(x), sd(x))

# plot distribution of exact heights in data
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")

# probabilities in actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

# probabilities in normal approximation match well
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

# probabilities in actual data over other ranges don't match normal approx as well
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))

library(tidyverse)
x <- seq(-4, 4, length = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x, f)) +
  geom_line()
dnorm(2, 0,1)

# define x as male heights from dslabs data
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

# generate simulated height data using normal distribution - both datasets should have n observations
n <- length(x)
avg <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n, avg, s)

# plot distribution of simulated_heights
data.frame(simulated_heights = simulated_heights) %>%
  ggplot(aes(simulated_heights)) +
  geom_histogram(color="black", binwidth = 2)

# define x as male heights from dslabs data
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

# generate simulated height data using normal distribution - both datasets should have n observations
n <- length(x)
avg <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n, avg, s)

# plot distribution of simulated_heights
data.frame(simulated_heights = simulated_heights) %>%
  ggplot(aes(simulated_heights)) +
  geom_histogram(color="black", binwidth = 2)

B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(800, avg, s)    # generate 800 normally distributed random heights
  max(simulated_data)    # determine the tallest height
})
mean(tallest >= 7*12)    # proportion of times that tallest person exceeded 7 feet (84 inches)

x <- seq(-4, 4, length.out = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x,f)) +
  geom_line()

#Assessment
RNGkind(sample.kind = "Rounding")
set.seed(16,sample.kind="Rounding")
act_scores <- rnorm(10000,20.9,5.7)
mean(act_scores)
sd(act_scores)
sum(act_scores>=36)
10000*(1-pnorm(36,mean(act_scores),sd(act_scores)))
1-pnorm(30,mean(act_scores),sd(act_scores))
pnorm(10,mean(act_scores),sd(act_scores))
x <- seq(1:36)
f_x <- dnorm(x,20.9,5.7)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x,f_x)) +
  geom_line()

z_act_scores <- (act_scores - mean(act_scores))/sd(act_scores)
1- pnorm(2)
(2*sd(act_scores))+mean(act_scores)
qnorm(0.975,mean(act_scores),sd(act_scores))

x <- seq(1:36)
cdf <- sapply(1:36, function (x){
  mean(act_scores <= x)
})
min(which(cdf>=0.95))
qnorm(0.95,20.9,5.7)

p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores,p)
sample_quantiles
names(sample_quantiles[max(which(sample_quantiles < 26))])
theoretical_quantiles <- qnorm(p,20.9,5.7)
qplot(theoretical_quantiles,sample_quantiles) + geom_abline()

#Random variables
# define random variable x to be 1 if blue, 0 otherwise
beads <- rep(c("red", "blue"), times = c(2, 3))
x <- ifelse(sample(beads, 1) == "blue", 1, 0)

# demonstrate that the random variable is different every time
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)

# sampling model 1: define urn, then sample
color <- rep(c("Black", "Red", "Green"), c(18, 18, 2)) # define the urn for the sampling model
n <- 1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)
X[1:10]

# sampling model 2: define urn inside sample function by noting probabilities
x <- sample(c(-1, 1), n, replace = TRUE, prob = c(9/19, 10/19))    # 1000 independent draws
S <- sum(x)    # total winnings = sum of draws
S

n <- 1000    # number of roulette players
B <- 10000    # number of Monte Carlo experiments
S <- replicate(B, {
  X <- sample(c(-1,1), n, replace = TRUE, prob = c(9/19, 10/19))    # simulate 1000 spins
  sum(X)    # determine total profit
})

mean(S < 0)    # probability of the casino losing money

library(tidyverse)
s <- seq(min(S), max(S), length = 100)    # sequence of 100 values across range of S
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S))) # generate normal density for S
data.frame (S = S) %>%    # make data frame of S for histogram
  ggplot(aes(S, ..density..)) +
  geom_histogram(color = "black", binwidth = 10) +
  ylab("Probability") +
  geom_line(data = normal_density, mapping = aes(s, f), color = "blue")

# Assessment
0.2*1 + 0.8*-.25
mean <- 44*(0.2*1 + 0.8*-.25)
sd <- abs(1-(-0.25))*sqrt(0.8*0.2)*sqrt(44)
1-pnorm(8,mean,sd)

set.seed(21,sample.kind = "Rounding")
S <- replicate(10000, {
  X <- sample(c(1,-.25),44,prob=c(0.2,0.8),replace=TRUE)
  sum(X)
})
mean(S>8)

44*(0.25*1 + 0.75*0)


p <- seq(0.25, 0.95, 0.05)
sample <- 1- pnorm(35,44*(p*1 +(1-p)*0),abs(1-0)*sqrt(p*(1-p)*sqrt(44)))
sample
p[min(which(sample > 0.8))]

p <- 5/38
mean <- (6*p)+(-1*(1-p))
sd <-abs(6-(-1))*sqrt(p*(1-p))
S <- replicate(500, {
  X <- ((6*p)+(-1*(1-p)))
  mean(X)
})
mean(S)

  abs(6-(-1))*sqrt(p*(1-p))/sqrt(500)
mean <- 500*((6*p)+(-1*(1-p)))

# Interest Rates
n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02
defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE)
sum(defaults * loss_per_foreclosure)

B <- 10000
losses <- replicate(B, {
  defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE) 
  sum(defaults * loss_per_foreclosure)
})

library(tidyverse)
data.frame(losses_in_millions = losses/10^6) %>%
  ggplot(aes(losses_in_millions)) +
  geom_histogram(binwidth = 0.6, col = "black")

n*(p*loss_per_foreclosure + (1-p)*0)    # expected value 
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))    # standard error
sd <- abs(6-(-1))*sqrt(p*(1-p))*sqrt(500)
pnorm(0,mean,sd)
x = - loss_per_foreclosure*p/(1-p)
x
x/180000

l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))\x
x/180000    # interest rate
loss_per_foreclosure*p + x*(1-p)    # expected value of the profit per loan
n*(loss_per_foreclosure*p + x*(1-p)) # expected value of the profit over n loans

B <- 100000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)    # expected value of the profit over n loans
mean(profit<0)    # probability of losing money

p <- .04
loss_per_foreclosure <- -200000
r <- 0.05
x <- r*180000
loss_per_foreclosure*p + x*(1-p)

z <- qnorm(0.01)
l <- loss_per_foreclosure
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)
n    # number of loans required
n*(loss_per_foreclosure*p + x * (1-p))    # expected profit over n loans

B <- 10000
p <- 0.04
x <- 0.05 * 180000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)

p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-new_p, new_p), replace = TRUE)
  sum(draws)
})
mean(profit)    # expected profit
mean(profit < 0)    # probability of losing money
mean(profit < -10000000)    # probability of losing over $10 million



## Inference and modeling course
library(tidyverse)
library(dslabs)
take_poll(25)    # draw 25 beads

X_hat <- 0.48
se <- sqrt(X_hat*(1-X_hat)/25)
pnorm(0.01/se) - pnorm(-0.01/se)

p <- 0.45    # unknown p to estimate
N <- 1000

# simulate one poll of size N and determine x_hat
x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
x_hat <- mean(x)

# simulate B polls of size N and determine average x_hat
B <- 10000    # number of replicates
N <- 1000    # sample size per replicate
x_hat <- replicate(B, {
  x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  mean(x)
})

library(tidyverse)
library(gridExtra)
p1 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(x_hat)) +
  geom_histogram(binwidth = 0.005, color = "black")
p2 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(sample = x_hat)) +
  stat_qq(dparams = list(mean = mean(x_hat), sd = sd(x_hat))) +
  geom_abline() +
  ylab("X_hat") +
  xlab("Theoretical normal")
grid.arrange(p1, p2, nrow=1)

library(tidyverse)
N <- 100000
p <- seq(0.35, 0.65, length = 100)
SE <- sapply(p, function(x) 2*sqrt(x*(1-x)/N))
data.frame(p = p, SE = SE) %>%
  ggplot(aes(p, SE)) +
  geom_line()

data("nhtemp")
data.frame(year = as.numeric(time(nhtemp)), temperature = as.numeric(nhtemp)) %>%
  ggplot(aes(year, temperature)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Average Yearly Temperatures in New Haven")
  
p <- 0.45
N <- 1000
X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))    # generate N observations
X_hat <- mean(X)    # calculate X_hat
SE_hat <- sqrt(X_hat*(1-X_hat)/N)    # calculate SE_hat, SE of the mean of N observations
c(X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # build interval of 2*SE above and below mean

z <- qnorm(0.995)    # calculate z to solve for 99% confidence interval
pnorm(qnorm(0.995))    # demonstrating that qnorm gives the z value for a given probability
pnorm(qnorm(1-0.995))    # demonstrating symmetry of 1-qnorm
pnorm(z) - pnorm(-z)    # demonstrating that this z value gives correct probability for interval

B <- 10000
inside <- replicate(B, {
  X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  between(p, X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # TRUE if p in confidence interval
})
mean(inside)

N <- 25
X_hat <- 0.48
(2*X_hat - 1) + c(-2, 2)*2*sqrt(X_hat*(1-X_hat)/N)

N <- 100    # sample size
z <- sqrt(N) * 0.02/0.5    # spread of 0.02
1 - (pnorm(z) - pnorm(-z))

d <- 0.039
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d+1)/2

# calculate confidence intervals of the spread
confidence_intervals <- sapply(Ns, function(N){
  X <- sample(c(0,1), size=N, replace=TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  2*c(X_hat, X_hat - 2*SE_hat, X_hat + 2*SE_hat) - 1
})

# generate a data frame storing results
polls <- data.frame(poll = 1:ncol(confidence_intervals),
                    t(confidence_intervals), sample_size = Ns)
names(polls) <- c("poll", "estimate", "low", "high", "sample_size")
polls

d_hat <- polls %>%
  summarize(avg = sum(estimate*sample_size) / sum(sample_size)) %>%
  .$avg

p_hat <- (1+d_hat)/2
moe <- 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size))   
round(d_hat*100,1)
round(moe*100, 1)

library(dslabs)
data(polls_us_election_2016)
names(polls_us_election_2016)

# keep only national polls from week before election with a grade considered reliable
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade)))

# add spread estimate
polls <- polls %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# compute estimated spread for combined polls
d_hat <- polls %>%
  summarize(d_hat = sum(spread * samplesize) / sum(samplesize)) %>%
  .$d_hat

# compute margin of error
p_hat <- (d_hat+1)/2
moe <- 1.96 * 2 * sqrt(p_hat*(1-p_hat)/sum(polls$samplesize))

# histogram of the spread
polls %>%
  ggplot(aes(spread)) +
  geom_histogram(color="black", binwidth = .01)

# number of polls per pollster in week before election
polls %>% group_by(pollster) %>% summarize(n())

# plot results by pollsters with at least 6 polls
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  ggplot(aes(pollster, spread)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# standard errors within each pollster
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  summarize(se = 2 * sqrt(p_hat * (1-p_hat) / median(samplesize)))

# collect last result before the election for each pollster
one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%      # keep latest poll
  ungroup()

# histogram of spread estimates
one_poll_per_pollster %>%
  ggplot(aes(spread)) + geom_histogram(binwidth = 0.01)

# construct 95% confidence interval
results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)
round(results*100, 1)


prev <- 0.00025    # disease prevalence
N <- 100000    # number of tests
outcome <- sample(c("Disease", "Healthy"), N, replace = TRUE, prob = c(prev, 1-prev))

N_D <- sum(outcome == "Disease")    # number with disease
N_H <- sum(outcome == "Healthy")    # number healthy

# for each person, randomly determine if test is + or -
accuracy <- 0.99
test <- vector("character", N)
test[outcome == "Disease"] <- sample(c("+", "-"), N_D, replace=TRUE, prob = c(accuracy, 1-accuracy))
test[outcome == "Healthy"] <- sample(c("-", "+"), N_H, replace=TRUE, prob = c(accuracy, 1-accuracy))

table(outcome, test)

library(tidyverse)
library(dslabs)
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%
  ungroup()

results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)

mu <- 0
tau <- 0.035
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))

posterior_mean
posterior_se

# 95% credible interval
posterior_mean + c(-1.96, 1.96)*posterior_se

# probability of d > 0
1 - pnorm(0, posterior_mean, posterior_se)

J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
X <- d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))

I <- 5
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
X <- sapply(1:I, function(i){
  d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
})

I <- 5
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
h <- rnorm(I, 0, 0.025)    # assume standard error of pollster-to-pollster variability is 0.025
X <- sapply(1:I, function(i){
  d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
})

mu <- 0
tau <- 0.035
sigma <- sqrt(results$se^2 + .025^2)
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))

1 - pnorm(0, posterior_mean, posterior_se)

library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
head(results_us_election_2016)

results_us_election_2016 %>% arrange(desc(electoral_votes)) %>% top_n(5, electoral_votes)

results <- polls_us_election_2016 %>%
  filter(state != "U.S." &
           !grepl("CD", state) &
           enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  group_by(state) %>%
  summarize(avg = mean(spread), sd = sd(spread), n = n()) %>%
  mutate(state = as.character(state))

# 10 closest races = battleground states
results %>% arrange(abs(avg))

# joining electoral college votes and results
results <- left_join(results, results_us_election_2016, by="state")

# states with no polls: note Rhode Island and District of Columbia = Democrat
results_us_election_2016 %>% filter(!state %in% results$state)

# assigns sd to states with just one poll as median of other sd values
results <- results %>%
  mutate(sd = ifelse(is.na(sd), median(results$sd, na.rm = TRUE), sd))

mu <- 0
tau <- 0.02
results %>% mutate(sigma = sd/sqrt(n),
                   B = sigma^2/ (sigma^2 + tau^2),
                   posterior_mean = B*mu + (1-B)*avg,
                   posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2))) %>%
  arrange(abs(posterior_mean))

mu <- 0
tau <- 0.02
clinton_EV <- replicate(1000, {
  results %>% mutate(sigma = sd/sqrt(n),
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV > 269)    # over 269 votes wins election

# histogram of outcomes
data.frame(clintonEV) %>%
  ggplot(aes(clintonEV)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 269)

mu <- 0
tau <- 0.02
bias_sd <- 0.03
clinton_EV_2 <- replicate(1000, {
  results %>% mutate(sigma = sqrt(sd^2/(n) + bias_sd^2),    # added bias_sd term
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV_2 > 269)    # over 269 votes wins election

# select all national polls by one pollster
one_pollster <- polls_us_election_2016 %>%
  filter(pollster == "Ipsos" & state == "U.S.") %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# the observed standard error is higher than theory predicts
se <- one_pollster %>%
  summarize(empirical = sd(spread),
            theoretical = 2*sqrt(mean(spread)*(1-mean(spread))/min(samplesize)))
se

# the distribution of the data is not normal
one_pollster %>% ggplot(aes(spread)) +
  geom_histogram(binwidth = 0.01, color = "black")

polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ggplot(aes(enddate, spread)) +
  geom_smooth(method = "loess", span = 0.1) +
  geom_point(aes(color = pollster), show.legend = FALSE, alpha = 0.6)

polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  select(enddate, pollster, rawpoll_clinton, rawpoll_trump) %>%
  rename(Clinton = rawpoll_clinton, Trump = rawpoll_trump) %>%
  gather(candidate, percentage, -enddate, -pollster) %>%
  mutate(candidate = factor(candidate, levels = c("Trump", "Clinton"))) %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  ggplot(aes(enddate, percentage, color = candidate)) +
  geom_point(show.legend = FALSE, alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.15) +
  scale_y_continuous(limits = c(30, 50))

z <- qt(0.975, nrow(one_poll_per_pollster) - 1)
one_poll_per_pollster %>%
  summarize(avg = mean(spread), moe = z*sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - moe, end = avg + moe)

# quantile from t-distribution versus normal distribution
qt(0.975, 14)    # 14 = nrow(one_poll_per_pollster) - 1
qnorm(0.975)

# load and inspect research funding rates object
library(tidyverse)
library(dslabs)
data(research_funding_rates)
research_funding_rates

# compute totals that were successful or not successful
totals <- research_funding_rates %>%
  select(-discipline) %>%
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men,
            no_men = applications_men - awards_men,
            yes_women = awards_women,
            no_women = applications_women - awards_women)

# compare percentage of men/women with awards
totals %>% summarize(percent_men = yes_men/(yes_men + no_men),
                     percent_women = yes_women/(yes_women + no_women))

tab <- matrix(c(3,1,1,3), 2, 2)
rownames(tab) <- c("Poured Before", "Poured After")
colnames(tab) <- c("Guessed Before", "Guessed After")
tab

# p-value calculation with Fisher's Exact Test
fisher.test(tab, alternative = "greater")

# compute overall funding rate
funding_rate <- totals %>%
  summarize(percent_total = (yes_men + yes_women) / (yes_men + no_men + yes_women + no_women)) %>%
  .$percent_total
funding_rate

# construct two-by-two table for observed data
two_by_two <- tibble(awarded = c("no", "yes"),
                     men = c(totals$no_men, totals$yes_men),
                     women = c(totals$no_women, totals$yes_women))
two_by_two

# compute null hypothesis two-by-two table
tibble(awarded = c("no", "yes"),
       men = (totals$no_men + totals$yes_men) * c(1-funding_rate, funding_rate),
       women = (totals$no_women + totals$yes_women) * c(1-funding_rate, funding_rate))

# chi-squared test
chisq_test <- two_by_two %>%
  select(-awarded) %>%
 chisq.test()
chisq_test$p.value

# odds of getting funding for men
odds_men <- (two_by_two$men[2] / sum(two_by_two$men)) /
  (two_by_two$men[1] / sum(two_by_two$men))

# odds of getting funding for women
odds_women <- (two_by_two$women[2] / sum(two_by_two$women)) /
  (two_by_two$women[1] / sum(two_by_two$women))

# odds ratio - how many times larger odds are for men than women
odds_men/odds_women

# multiplying all observations by 10 decreases p-value without changing odds ratio
two_by_two %>%
  select(-awarded) %>%
  mutate(men = men*10, women = women*10) %>%
  chisq.test()