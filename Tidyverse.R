library(tidyverse)

library(readr)
rne <- read_csv("Data/Repertoire-national-des-elus.csv")

spec(rne) #to see all information about the dataset

#the pipe: whatever is right of the the pipe and feed it to the right side of the pipe. You can create a pipeline. The result of the function can be included into the other ones.
# %>% #Ctrl Shift M
  
rne %>% 
  filter(`Code sexe` %in% "F") %>%  #with %in% you can use more values, with == only one value, 2. reason, %in% does not return NA values
  mutate(`Code sexe`= recode(`Code sexe`, "F" = "Female", "M" = "Male")) %>% #We want to transform a column --> mutate
  arrange(`Date de naissance`) %>% 
  #select(-`Code profession`) %>% 
  group_by(`Libellé de la profession`) %>% #counts the different amount of proffessions named in the data
  summarise(n=n(), age = mean(Age), sd_age = sd(Age)) %>% #says how many persons there are per group.
  arrange(desc(n))

rne %>%  # we want to gather the data.. from large to long
  gather("mandat", "value", `Conseiller Municipal`:Maire) %>% #defining the key value, if more offices are held, for example "mandat", and collecting only the columns that we need
  filter(value %in% "true") %>%
  filter(!(`Date de naissance` %in% lubridate::ymd("1900-01-01"))) %>% # ! as the NOT-Function in R
  select(-value) %>% 
  group_by(mandat) %>% 
  summarise(n=n(), age = mean(Age, na.rm = TRUE)) %>% 
  arrange(desc(n))

rne %>%  # we want to gather the data.. from large to long
  gather("mandat", "value", `Conseiller Municipal`:Maire) %>% #defining the key value, if more offices are held, for example "mandat", and collecting only the columns that we need
  filter(value %in% "true") %>% 
  select(-value) %>% 
  group_by(mandat) %>% 
  filter(is.na(Age)) %>% 
  summarise(n=n())

rne %>%  # we want to gather the data.. from large to long
  gather("mandat", "value", `Conseiller Municipal`:Maire) %>% #defining the key value, if more offices are held, for example "mandat", and collecting only the columns that we need
  filter(value %in% "true") %>% 
  select(-value) %>% 
  group_by(mandat) %>% 
  filter(`Date de naissance` %in% lubridate::ymd("1900-01-01")) %>% #checking how many 
  summarise(n=n())

rne %>%  # we want to gather the data.. from large to long
  gather("mandat", "value", `Conseiller Municipal`:Maire) %>% #defining the key value, if more offices are held, for example "mandat", and collecting only the columns that we need
  filter(value %in% "true") %>% 
  select(-value) %>%
  group_by(Identifiant) %>% 
  summarise(offices = n(), occupation = unique(`Libellé de la profession`), gender = unique(`Code sexe`)) %>% #unique gives back only one value, because all of them are the same for occupation and gender
  ungroup() %>% 
  group_by(occupation, gender) %>% 
  summarise(offices = mean(offices)) %>% 
  arrange(desc(offices))

rne %>%
  filter(`Code sexe` %in% "F") %>% 
  group_by(`Libellé de la profession`) %>% 
  arrange(Age) %>% #by defaut Age is in ascending order
  slice(2) %>% #keeping only the 2nd row
  View()

rne %>%
  filter(`Code sexe` %in% "F") %>% 
  group_by(`Libellé de la profession`) %>% 
  arrange(desc(Age)) %>%
  mutate(rank=1:n()) %>% 
  filter(rank %in% 2) %>% 
  View
  
rne %>%
  mutate(number = case_when(`Nombre de mandats` %in% 1 ~"one",
                           `Nombre de mandats` %in% 2 ~"two",
                           `Nombre de mandats` %in% 2 ~"three",
                           `Nombre de mandats` %in% 2 ~"four",
                           TRUE ~ NA_character_)) %>% 
  View
                           
                           
#Inner Join: Keeping only the observation that are present in both Datasets
#Outer Join: Left (Left Dataset wins), 3 is missing in the y tabel --> NA, Right the opposite, Full, keeping both and filling with NAs
#Left Join the most common one, because you want to keep the Master table and fill it with additional information
#Semi Join: Only keeping observation on the left, that are available on the left
#Anti Join: Only keeping the observation that is not present on the second table

#Binding different from joining.
#Binding = Same kind of dataset that have the same columns but different observation --> bind_rows
#bind_cols --> same observation but different columns

#count() as the same as group by and summarise and arrange together

########################### Visualization : GG Plot ########################################

library(ggplot2)
rne %>%
  ggplot(aes(x=`Code sexe`)) +
  geom_bar() +
  scale_y_continuous(labels = scales::comma)

library(hrbrthemes)

cairo_pdf(file= "./plot1.pdf", width=7, height = 12)
rne %>%
  count(`Libellé de la profession`, sort=TRUE) %>%
  filter(!is.na(`Libellé de la profession`)) %>% 
  arrange(n) %>% 
  filter(n>1000) %>% 
  mutate(occupation = fct_inorder(`Libellé de la profession`)) %>%
  mutate(coord=if_else(n>40000, n-1000, n+1000),
         colour=if_else(n>40000, "white", "black")) %>% 
  ggplot(aes(x=occupation, y=n)) +
  geom_bar(stat = "identity", width = 0.6, fill="#391296") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip()+
  geom_text(aes(label=occupation, y = coord, colour=colour), hjust="inward", vjust="center", size=2) +
  scale_colour_manual(values=c("#391296", "white"), guide=FALSE)+
  xlab("") +
  ylab("Frequency") +
  ylim(c(0,70000)) +
  scale_x_discrete(labels=NULL) +
  theme(axis.ticks.y=element_blank())+
  theme_ipsum(grid="X")+
  labs(title="Most elected officials are employees, farmers or retired", subtitle= "Number of elected officials in France in 2018 by occupation", caption="Source: RNE (Ministère de l'intérieur), computation by Sciences Po students")
dev.off  


#Small Multiplies

cairo_pdf(file= "./plot1.pdf", width=12, height = 7)
rne %>% 
  mutate(gender = recode(`Code sexe`, "M" = "Male", "F" = "Female")) %>% 
  count(`Libellé de la profession`,gender, sort = TRUE) %>% 
  filter(!is.na(`Libellé de la profession`)) %>% 
  ungroup %>% 
  arrange(gender, n) %>% 
  filter(n > 1000) %>% 
  mutate(order = row_number()) %>% 
  mutate(occupation = fct_inorder(`Libellé de la profession`)) %>% 
  # creating new var coord so that if n past a certain limit, the label will be 
  # inside the bar and white. otherwise will be black
  mutate(coord = if_else(n > 22000, n - 1000, n + 1000),
         colour = if_else(n > 22000, "white", "black")) %>% 
  ggplot(aes(x = order, y = n)) +
  geom_bar(aes(fill = gender), stat = "identity", width = 0.8) +
  scale_fill_discrete( guide = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  # geom_text() = creating new geometry of type 'text' onto plot
  #     don't have to indicate x position as it was indicated in initial ggplot
  #     have to change y position because we want labels 
  #     y indicates the CENTER of the label 
  #    hjust = horizontal justification 
  geom_text(aes(label = occupation, y = coord, colour = colour), hjust = "inward", 
            vjust = "center", size = 2) +
  # set colours of text. remove legend
  scale_color_manual(values = c("black", "white"), guide = FALSE) +
  # facet wrap
  facet_wrap(facets = vars(gender), scales = "free_y") +
  # using xlab because coord_flip flipped x and y
  # removing x axis label
  xlab("") +
  ylab("") +
  scale_x_discrete(labels = NULL) +
  theme(axis.ticks.y = element_blank()) +
  theme_ipsum(grid = "X") +
  labs(title = "Most elected officials are employees, farmers or retired.", subtitle = "Number of elected officials in France in 2018 by occupation.", caption = "Source: RNE (Ministère de l'intérieur), computation by Sciences Po students.")
dev.off()


########################### MAPPING ############################################

install.packages("sf")
install.packages("mapview")
install.packages("leaflet")
install.packages("tmap")

library(sf)
library(mapview)
library(leaflet)
library(tmap)

toilets<-read_sf("./data/sanisettesparis2011.geojson")
toilets

mapview(toilets)

