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

rne %>%
  count(`Libellé de la profession`, sort=TRUE) %>%
  mutate(occupation= factor(`Libellé de la profession`)) %>% 
  ggplot(aes(x=`cccupation`, y=n)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip()
 