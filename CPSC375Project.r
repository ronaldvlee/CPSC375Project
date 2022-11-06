# reading data
time_series_covid19_vaccine_doses_admin_global <- read_csv("https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/global_data/time_series_covid19_vaccine_doses_admin_global.csv")
hospital_beds <- read_csv("hospitaldata.csv")
demographics <- read_csv("demographics.csv")

# wrangled demographics data
demographics <- demographics %>% select(-c(`Series Name`,`Country Code`)) %>% pivot_wider(names_from=`Series Code`, values_from = `YR2015`)

demographics <- demographics %>% mutate(
  SP.POP.80UP=SP.POP.80UP.FE+SP.POP.80UP.MA,
  SP.POP.1564.IN=SP.POP.1564.MA.IN+SP.POP.1564.FE.IN,
  SP.POP.0014.IN=SP.POP.0014.MA.IN+SP.POP.0014.FE.IN,
  SP.DYN.AMRT=SP.DYN.AMRT.MA + SP.DYN.AMRT.FE,
  SP.POP.TOTL.IN=SP.POP.TOTL.MA.IN+SP.POP.TOTL.FE.IN,
  SP.POP.65UP.IN=SP.POP.65UP.MA.IN+SP.POP.65UP.FE.IN,
  .keep="unused"
)

# removed unnecessary variables, filtered out all Province level reported data
time_series_covid19_vaccine_doses_admin_global <- time_series_covid19_vaccine_doses_admin_global %>% 
  filter(is.na(Province_State)) %>% 
  select(-c(Province_State,FIPS,Admin2,UID,iso2,iso3,code3,Lat,Long_,Combined_Key)) %>%
  # pivoting data frame to be longer with the dates and getting the vaccinated population
  pivot_longer(cols = starts_with("202"), values_drop_na = TRUE, names_to = "date", values_to = "Vaccinated Population") %>%
  filter(`Vaccinated Population` != 0) %>% 
  # getting vaccination rate
  mutate(vacRate = `Vaccinated Population`/Population)

# calculating the days from the start date
time_series_covid19_vaccine_doses_admin_global <- time_series_covid19_vaccine_doses_admin_global %>% 
  mutate(`date`=as.Date(`date`, format = "%Y-%m-%d")) %>%
  group_by(Country_Region) %>% 
  mutate(days_from_start = as.numeric(difftime(`date`, min(`date`), units="days")) + 1) %>%
  select(-`date`)
  

# removing all hospital bed data that is not of the latest year, dropping the year
hospital_beds <- hospital_beds %>% group_by(Country) %>% filter(Year == max(Year)) %>% select(-`Year`)

# joining hospital beds to demographics
demographics <- demographics %>% inner_join(hospital_beds, by = c("Country Name"="Country"))

# renaming column names
time_series_covid19_vaccine_doses_admin_global <- time_series_covid19_vaccine_doses_admin_global %>% 
  inner_join(demographics, by = c("Country_Region" = "Country Name")) %>%
  rename(Country = Country_Region, beds = `Hospital beds (per 10 000 population)`) %>%
  relocate(beds, .after = vacRate) %>%
  relocate(vacRate,`Vaccinated Population`,.after = Country) %>%
  filter(!is.na(vacRate)) %>%
  
  # fixing Country names
  mutate(Country = replace(Country, Country == "Republic of Korea", "South Korea")) %>%
  mutate(Country = replace(Country, Country == "Korea, Rep.", "South Korea")) %>%
  mutate(Country = replace(Country, Country == "Iran (Islamic Republic of)", "Iran")) %>%
  mutate(Country = replace(Country, Country == "Iran, Islamic Rep.", "Iran")) %>%
  mutate(Country = replace(Country, Country == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom"))