data <- read.csv("world-happiness-report-2021.csv")
data_main <- read.csv("world-happiness-report-2021.csv")
head(data)
show(data)

colnames(data_main)[1] = "Country.name"

# czcionki
library(tidyverse)
install.packages("extrafont")
install.packages("cowplot")
library(extrafont)
library(cowplot)
font_import() # import - wystarczy raz
loadfonts(device = "win", quiet = TRUE) # zaladuj - za kazdym razem


hist(data$Ladder.score, breaks = 10)

# histogram - szczescie od reginu
ggplot(data, aes(x=Ladder.score, fill=Regional.indicator))+
  geom_histogram(breaks = seq(min(data$Ladder.score, na.rm = TRUE),
                              max(data$Ladder.score, na.rm = TRUE)+1, 0.5), color = "black", closed = "left")+
  scale_x_continuous(breaks = seq(1, 10, 2))+
  labs(x = "Wskaźnik szczęścia", y = "Częstość", title = "Wskaźnik szczęścia w zależności od regionu")+
  scale_linetype_discrete(name = "Fancy Title")+
  facet_wrap(~Regional.indicator, scales = "free_y")+
  theme(legend.position = "none", text=element_text(size = 11, family = "Cambria"))

windowsFonts()

# histogram - szczescie globalnie
ggplot(data, aes(x=Ladder.score))+
  geom_histogram(breaks = seq(min(data$Ladder.score, na.rm = TRUE),
                              max(data$Ladder.score, na.rm = TRUE)+1, 0.5), color = "black",fill = "chartreuse4", closed = "left")+
  scale_x_continuous(breaks = seq(1, 10, 0.5))+
  labs(x = "Wskaźnik szczęścia", y = "Częstość", title = "Globalne wartości wskaźnika szczęścia")+
  theme(text = element_text(size = 11, family = "Cambria"))

# usuwanie niepotrzebnych kolumn
head(data)
data <- data %>%
  select(-Standard.error.of.ladder.score)
  
data <- data %>%
  select(-upperwhisker, -lowerwhisker) 
head(data)  

data <- data %>%
  select(-Standard.error.of.ladder.score)

data <- data%>%
  select(-Ladder.score.in.Dystopia, -Explained.by..Log.GDP.per.capita, -Explained.by..Freedom.to.make.life.choices,
        -Explained.by..Social.support, -Explained.by..Healthy.life.expectancy)
data <- data%>%
  select(-Explained.by..Generosity, -Explained.by..Perceptions.of.corruption)
data <- data%>%
  select(-Dystopia...residual)
data <- data %>%
  select(-Generosity)

colnames(data)[1] = "Country.name" # zmiana nazwy kolumny nazw panstw
# znajdz nazwy zmiennych dla minimalnych wartosci
# szczescie
summary(data$Ladder.score)
min_ladder <- min(data$Ladder.score) 
which.min(data$Ladder.score)
data$Ladder.score[149]
data[data$Ladder.score == min_ladder, "Country.name"]

max_ladder <- max(data$Ladder.score) # szczescie
which.max(data$Ladder.score)
data$Ladder.score[1]
data[data$Ladder.score == max_ladder, "Country.name"]

# przew. dlugosc zycia
summary(data$Healthy.life.expectancy)
min_life <- min(data$Healthy.life.expectancy)
max_life <- max(data$Healthy.life.expectancy)
data[data$Healthy.life.expectancy == min_life, "Country.name"]
data[data$Healthy.life.expectancy == max_life, "Country.name"]

head(data)
# wsparcie socjalne panstwa
summary(data$Social.support)
min_support <- min(data$Social.support)
max_support <- max(data$Social.support)
data[data$Social.support == min_support, "Country.name"]
data[data$Social.support == max_support, "Country.name"]

# pkb
summary(data$Logged.GDP.per.capita)
min_gpd <- min(data$Logged.GDP.per.capita)
max_gpd <- max(data$Logged.GDP.per.capita)
data[data$Logged.GDP.per.capita == min_gpd, "Country.name"]
data[data$Logged.GDP.per.capita == max_gpd, "Country.name"]

summary(data_main$Freedom.to.make.life.choices)
min_free <- min(data_main$Freedom.to.make.life.choices)
max_free <- max(data_main$Freedom.to.make.life.choices)
data_main[data_main$Freedom.to.make.life.choices == min_free, "Country.name"]
data_main[data_main$Freedom.to.make.life.choices == max_free, "Country.name"]

# ramka wasy dla ladder score
IQR <- IQR(data$Ladder.score, na.rm = TRUE)
Q1 <- quantile(data$Ladder.score, .25)
Q2 <- quantile(data$Ladder.score, .75)
data$Ladder.score[data$Ladder.score<(Q1-1.5*IQR)] <- median(data$Ladder.score)
data$Ladder.score[data$Ladder.score>(Q2+1.5*IQR)] <- median(data$Ladder.score)

ggplot(data, aes(x = Ladder.score))+
  geom_boxplot(fill = "chartreuse4", outlier.color = "red", outlier.shape = 4, coef = 1.5)+
  labs(xlab = "Rozproszenie",title = "Rozproszenie zmiennej Ladder Score")+
  theme(text=element_text(size = 11, family = "Cambria"))
  

# przedzial ufonsci dla wartosci sredniej (przewidywanej)
mean(data$Ladder.score)
sd(data$Ladder.score)
pnorm
nrow(data) # ilosc rzedow w data
round(5.533+c(-1,1)*1.073*pnorm(0.975)/sqrt(nrow(data)),3)

# rozklad normalny ladder.score
srednia <- mean(data$Ladder.score)
srednia
odchylenie <- sd(data$Ladder.score)

ggplot(data, aes(x=Ladder.score))+
  geom_histogram(aes(y = ..density..),
                 breaks = seq(min(data$Ladder.score, na.rm = TRUE),
                              max(data$Ladder.score, na.rm = TRUE)+1, 0.5), color = "black",fill = "chartreuse4", closed = "left")+
  scale_x_continuous(breaks = seq(1, 10, 0.5))+
  labs(x = "Wskaźnik szczęścia", y = "Częstość", title = "Rozkład normalny oraz empiryczny Ladder Score")+
  theme(text = element_text(size = 11, family = "Cambria"))+
  stat_function(fun = dnorm, args = list(mean = srednia, sd = odchylenie), col = "red", lwd = 1.5, lty = "longdash")
  
# test rozkladu normalnego
qqnorm(data$Ladder.score)
qqline(data$Ladder.score, lty = "longdash", col = "red")
shapiro.test(data$Ladder.score)

# regresja
ladder <- c(data$Ladder.score)

library(GGally)
ggcorr(data, label = T)

colnames(data)[1] = "Ladder score"
colnames(data)[2] = "Lod. PKB per capita"

cbind(cor.test(data$Ladder.score, data$Logged.GDP.per.capita)$p.value,
      cor.test(data$Ladder.score, data$Social.support)[[3]],
      cor.test(data$Ladder.score, data$Healthy.life.expectancy)[[3]],
      cor.test(data$Ladder.score, data$Freedom.to.make.life.choices)[[3]],
      cor.test(data$Ladder.score, data$Generosity)[[3]],
      cor.test(data$Ladder.score, data$Perceptions.of.corruption)[[3]])


head(data)

model_all <- lm(Ladder.score~., data)
summary(model_all)

data <-data %>%
  select(-`Ladder score`)
data <- data %>%
  select(-`Lod. PKB per capita`)


data_new <- data
data_new <- data_new %>%
  select(-Freedom.to.make.life.choices, -Perceptions.of.corruption)

data_base <- data %>%
  select(-Perceptions.of.corruption) # ostateczny wektor DATA
head(data_base)

model_all <- lm(Ladder.score~., data_base)
summary(model_all)

model_back <- step(model_all, direction = "backward")
summary(model_back)

model_forward <- step(model_all, direction = "forward")
summary(model_forward)
colnames(data_base)[2]="PKB.per.capita"
colnames(data_base)[3]="Social.support"
colnames(data_base)[4]="Life.expentancy"
colnames(data_base)[5]="Freedom.of.choices"
ggcorr(data_base, label = T)

# analiza reszt
# losowosc odchylen
plot(data$Ladder.score, model_all$residuals,xlab = "ladder score", ylab = "reszty")
abline(h = 0, lty = 2, col = "red")
summary(model)
summary(model_all)
model <- model_all

# normalnosc rozkladu
qqnorm(model_all$residuals)
qqline(model_all$residuals, col = "red", lty = 2)
shapiro.test(model_all$residuals)

# nieobciazonosc
mean(model_all$residuals)

# homoscedastycznosc
library(lmtest)
bptest(model_all)

library(car)

vif(model_all)

summary(model)
