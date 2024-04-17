library(tidyverse)
library(ggplot2)
library(dplyr)
data<-read.csv("../data/433900.csv")

#1.Histrograma
names(data)=c("1","2","3","Pavadinimas","5","6","7","menuo","VidAtlyginimas","ApdraustiejiSk","11","12","13")
g1=data%>%
  ggplot(aes(x=VidAtlyginimas))+
  geom_histogram(bins = 200,fill='blue')+
  labs(title = 'Vidutinis darbo užmokestis',x='Darbo užmokestis',y='Kiekis')
ggsave('../img/pirmasGrafikas.png',g1)

#2. Penkios įmonės, kurių faktinis sumokėtas darbo užmokestis per metus buvo didžiausias

top5imones= data%>%
  group_by(Pavadinimas)%>%
  summarise(top=max(VidAtlyginimas))%>%
  arrange(desc(top))%>%
  head(5)
g2 <- data %>%
  filter(Pavadinimas%in% top5imones$Pavadinimas) %>%
  mutate(Menuo=ym(menuo))%>%
  ggplot(aes(x = Menuo, y = VidAtlyginimas, color = Pavadinimas)) +
  geom_line()
ggsave('../img/antrasGrafikas.png',g2)

#3. Išrinkti maksimalų apdraustų darbuotojų skaičių iš top 5 įmonių

apdraustieji <- data %>%
  filter(Pavadinimas %in% top5imones$Pavadinimas) %>%
  group_by(Pavadinimas) %>%
  summarise(maxapdraust=max(ApdraustiejiSk))%>%
  arrange(desc(maxapdraust))

apdraustieji$Pavadinimas=factor(apdraustieji$Pavadinimas,levels=apdraustieji$Pavadinimas[order(apdraustieji$maxapdraust,decreasing=TRUE)])


g3 <- apdraustieji%>%
  ggplot(aes(x=Pavadinimas,y=maxapdraust,fill=Pavadinimas))+
  geom_col()+labs(title="Maksimalus apdraustųjų skaičius",x="Įmonių pavadinimai",y="Apdraustųjų skaičius")
ggsave('../img/treciasGrafikas.png',g3)


