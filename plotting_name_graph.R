
library(tidyverse)
library(myGraphicsToolkit)
library(showtext)
library(janitor)

# Last data

emma_messy <- read.table("emma_navnestatistikk.csv", header = FALSE, sep = "")

emma <- emma_messy[ , 3:ncol(emma_messy)] %>% t() %>% as.data.frame() %>% 
  transmute(year = as.numeric(V1), percent = as.numeric(V2))


# Font
f1 <- "Open Sans"
f2 <- "IM Fell DW Pica"
font_add_google(name = f1, family = f1)
font_add_google(name = f2, family = f2)
showtext_auto()

col_bg <- "#E5F6E5"


ggplot(emma, aes(x = year, y = percent)) +
  geom_line(size = 1, lineend = "round") +
  scale_x_continuous(breaks = seq(1880, 2021, by = 10)) +
  labs(title = "Emmas through the ages",
       subtitle = expression(paste("Percentage of Norwegian babies named ", 
                                   italic("Emma"))),
       caption = "Source: ssb.no | Graphic: Emma Skarstein")+
  theme(text = element_text(family = f1, size = 12),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 8, margin = margin(t = 10, b = -10)),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = col_bg, color = col_bg),
        plot.background = element_rect(fill = col_bg, color = col_bg),
        panel.grid = element_blank(),
        plot.margin = margin(20, 20, 20, 20))


ggsave("emmaplot.png", height = 6, width = 6*1.61803398875)

emma_france <- read.csv("emma_france.csv", sep = ";") %>% clean_names()
emma_usa <- read.csv("emma_usa.csv", sep = ";") %>% clean_names()


names(emma_france)
ultimate_emma <- full_join(emma, emma_usa, by = "year") %>% 
  transmute(year = year, usa = x_percent_used, norway = percent) %>% 
  pivot_longer(cols = 2:3, names_to = "country", values_to = "percent")



ggplot(ultimate_emma, aes(x = year, y = percent)) +
  geom_line(aes(color = country), size = 1, lineend = "round") +
  scale_x_continuous(breaks = seq(1880, 2021, by = 10)) +
  labs(title = "Emmas through the ages",
       subtitle = expression(paste("Percentage of Norwegian babies named ", 
                                   italic("Emma"))),
       caption = "Source: ssb.no | Graphic: Emma Skarstein")+
  theme(text = element_text(family = f1, size = 12),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 8, margin = margin(t = 10, b = -10)),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = col_bg, color = col_bg),
        plot.background = element_rect(fill = col_bg, color = col_bg),
        panel.grid = element_blank(),
        plot.margin = margin(20, 20, 20, 20))




