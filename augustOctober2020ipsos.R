library(tidyverse)
library(glue)
data = read_csv("data/Aug_Oct_2020.csv") %>%
  rename(country = X.1,
         percent_august = "Total Agree - August 2020",
         percent_october = "Total Agree - October 2020") %>%
  mutate(offset_august = if_else(percent_august < percent_october,
                                 percent_august -2,
                                 percent_august +2),
         offset_october = if_else(percent_august > percent_october,
                                 percent_october -2,
                                 percent_october +2))

data %>%
  pivot_longer(cols = -country, names_to =c(".value", "month"), names_sep = "_",) %>%
  ggplot(aes(x=percent, y=country, color=month)) +
  geom_line(color="#e6e6e6", size=1.75) +
  geom_point(size=2, show.legend = FALSE) +
  geom_text(aes(label=paste(percent, "%", sep=""), x=offset), size=3, show.legend = FALSE) +
  scale_color_manual(name=NULL,
                      breaks=c("august", "october"),
                      values=c("#727272", "#15607a"),
                      labels=c("August", "October"))  +
  theme_classic()
  

ggsave("AugOct2020Ipsos.tiff", width = 6, height = 4)

# testString = "test"
# testString2 = paste(testString, "%", sep="")
# testString3 = glue("{testString}%")
# testString3
# dev.new
