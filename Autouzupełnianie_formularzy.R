#GDD 

ERA5 <- read.csv("/Users/maciejjurzyk/Downloads/TEMP2M_Recznie - Arkusz1-5.csv")

ERA5 %>%
  glimpse()

ERA53 <- ERA5 %>%
  mutate(
    First_10_Characters = as.Date(substr(Date, 1, 10), format = "%Y-%m-%d"),
    New_Date = as.POSIXct(First_10_Characters)
  ) %>%
  select(-Date) %>%
  glimpse()


###1.2.1 ERA5 Temperature to numeric ----

ERA5 <- ERA53%>%
  mutate(Temp = as.numeric(gsub(",", ".", Temp)))

ERA5 %>% glimpse() # full success! 


#### 1.2.2 GGPLOT----

## ERA5

#2020 

um_data2020 <- ERA5 %>%
  group_by(Category, year = as.integer(format(First_10_Characters, "%Y"))) %>%
  ungroup() %>%
  filter(year == 2020) %>%
  glimpse()

um_data2020 <- ERA5 %>%
mutate(First_10_Characters = as.Date(First_10_Characters), GDD = ifelse(Temp < 5, 0, Temp)) %>% 
  group_by(Category, year = as.integer(format(First_10_Characters, "%Y"))) %>%
  ungroup() %>%
  filter(year == 2020) %>%
  glimpse()

sum_data2020_1 <- um_data2020 %>%
  arrange(Category, First_10_Characters) %>%
  group_by(Category) %>%
  mutate(GDD = cumsum(GDD)) %>%
  ungroup() %>% 
  glimpse()

sum_data2020_2 <- sum_data2020_1 %>%
  mutate(
    DOY = as.numeric(First_10_Characters - as.Date(paste0(year(First_10_Characters), "-01-01")) + 1)
  ) %>% 
  glimpse()


#2021 

um_data2021 <- ERA5 %>%
  group_by(Category, year = as.integer(format(First_10_Characters, "%Y"))) %>%
  ungroup() %>%
  filter(year == 2021) %>%
  glimpse()

um_data2021 <- ERA5 %>%
  mutate(First_10_Characters = as.Date(First_10_Characters), GDD = ifelse(Temp < 5, 0, Temp)) %>% 
  group_by(Category, year = as.integer(format(First_10_Characters, "%Y"))) %>%
  ungroup() %>%
  filter(year == 2021) %>%
  glimpse()

sum_data2021_1 <- um_data2021 %>%
  arrange(Category, First_10_Characters) %>%
  group_by(Category) %>%
  mutate(GDD = cumsum(GDD)) %>%
  ungroup() %>% 
  glimpse()

sum_data2021_2 <- sum_data2021_1 %>%
  mutate(
    DOY = as.numeric(First_10_Characters - as.Date(paste0(year(First_10_Characters), "-01-01")) + 1)
  ) %>% 
  glimpse()
  


#2022

um_data2022 <- ERA5 %>%
  group_by(Category, year = as.integer(format(First_10_Characters, "%Y"))) %>%
  ungroup() %>%
  filter(year == 2022) %>%
  glimpse()

um_data2022 <- ERA5 %>%
  mutate(First_10_Characters = as.Date(First_10_Characters), GDD = ifelse(Temp < 5, 0, Temp)) %>% 
  group_by(Category, year = as.integer(format(First_10_Characters, "%Y"))) %>%
  ungroup() %>%
  filter(year == 2022) %>%
  glimpse()

sum_data2022_1 <- um_data2022 %>%
  arrange(Category, First_10_Characters) %>%
  group_by(Category) %>%
  mutate(GDD = cumsum(GDD)) %>%
  ungroup() %>% 
  glimpse()

sum_data2022_2 <- sum_data2022_1 %>%
  mutate(
    DOY = as.numeric(First_10_Characters - as.Date(paste0(year(First_10_Characters), "-01-01")) + 1)
  ) %>% 
  glimpse()


sum_data2021_1 %>% glimpse()


## Bindowanie tabelek

combined_data <- bind_rows(sum_data2020_2, sum_data2021_2, sum_data2022_2)


combined_data1 <-  combined_data %>% 
  mutate(PUNKT= Category)

  

combined_data1 %>% glimpse()



## nowa tabelka z pokosem 

pokos1 <- read.csv("/Users/maciejjurzyk/Downloads/Arkusz kalkulacyjny bez tytułu - Arkusz1.csv")

pokos1 %>% glimpse()

pokos2 <- pokos1 %>%
  mutate(
    First_10_Characters = as.Date(DATA, format = "%m/%d/%Y"),
    New_Date = as.POSIXct(First_10_Characters)
  ) %>%
  select(-DATA) %>%
  glimpse()

pokos_combined <- pokos2 %>%
  left_join(combined_data1, by = c("First_10_Characters", "PUNKT"))

pokos_combined %>% view()
