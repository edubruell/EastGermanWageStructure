library(pacman)
p_load(tidyverse,here,glue,lubridate,readxl,ags,wiesbaden)
#chat("I am only doing a small test if my openai API key is stored in my .RProfiles file and all needed packages for me communicating with you are here. 
#     Can you please reply with a short poem about the woes of an Rprogrammer updating his environment if everything is peachy?")


source(here("general_functions.R"))

`%nin%` <- Negate(`%in%`)
#Add your own destatis account here:
login <- c(user="RE004538", password="Zamofibup;57", db="regio")
#I can now succesfully login
test_login(login)


#Retrieve data from destatis and cache on disk
retrieve_cached_data <- function(.key,.login){
  
  cahed_tables <- dir(here("smalldata","cache_destatis"),pattern = "*.csv") %>%
    str_remove("\\.csv")
  
  if(.key %in% cahed_tables ){ out <- read_csv(here("smalldata","cache_destatis",glue("{.key}.csv")))}
  
  if(.key %nin% cahed_tables){ 
    out <- retrieve_data(tablename=.key, genesis=login)
    out %>% write_csv(here("smalldata","cache_destatis",glue("{.key}.csv")))
  }
  
  return(out)
}
#Most data is unfortunately not in the regional database but in the general data


#Get Population/Employment shares
d_popemp <- read_csv2(here("smalldata","12211-9004_flat.csv")) %>%
  transmute(bula = parse_number(`1_Auspraegung_Code`),
            year = if_else(Zeit_Label=="Jahr", 
                           parse_number(Zeit),
                           str_extract(Zeit,"[^\\/]*$") %>% 
                             parse_number()),
            east = if_else(bula>=1 & bula<=10, FALSE,TRUE),
            pop = BEV020__Bevoelkerung__1000*1000,
            emp = ERW002__Erwerbstaetige__1000*1000) %>%
  arrange(bula,year)%>%
  group_by(east,year) %>%
  summarise(across(emp|pop, sum),.groups="drop") %>%
  filter(year>=1995) %>%
  arrange(east,year) %>%
  mutate(east     = as.integer(east),
         emp_rate = emp/pop)

#Unemployment rate 
d_unemp <- read_csv2(here("smalldata","13211-0001_flat.csv"),locale = locale(encoding = "ISO-8859-1")) %>%
  transmute(east = as.integer(`1_Auspraegung_Code`=="DN"),
            year = Zeit,
            ue_rate = parse_number(ERW112__Arbeitslosenquote_aller_zivilen_Erwerbspersonen__Prozent, locale= locale(decimal_mark = ","))
            ) %>%
  filter(!is.na(east), year>=1995) %>%
  arrange(east,year)

#Real and nominal gdp per capita  
d_gdp <- read_csv2(here("smalldata","82111-0001-flat.csv"),locale = locale(encoding = "ISO-8859-1")) %>%
  filter(Zeit>=1995) %>%
  transmute(year       = Zeit,
            state = parse_number(`1_Auspraegung_Code`,na="DG"),
            state_name = `1_Auspraegung_Label`,
            nominal_gdp = BIP006__Bruttoinlandsprodukt_zu_Marktpreisen_i.jew.Preisen__Mill._EUR*10^6
            ) %>%
  filter(!is.na(state)) %>%
  arrange(year,state) %>%
  left_join(haven::read_dta(here("smalldata","cpi2017.dta")) %>%
              pivot_longer(cols = starts_with("state"),names_to="state",values_to = "cpi_factor", names_prefix = "state") %>%
              mutate(state = parse_number(state)),
            by=c("year","state")) %>%
  mutate(real_gdp = nominal_gdp*cpi_factor,
         east = if_else(state>=1 & state<=10, 0L,1L)) %>%
  group_by(east,year) %>%
  summarise(across(ends_with("gdp"),sum),.groups="drop") %>%
  left_join(d_popemp, by= c("east","year")) %>%
  transmute(east,year,real_gdp_pc = real_gdp/pop, nom_gdp_pc = nominal_gdp/pop) 

d_gdp %>%
  left_join(d_popemp %>% select(east,year,emp_rate), by= c("east","year")) %>%
  left_join(d_unemp  %>% mutate(ue_rate = ue_rate/100), by= c("east","year")) %>%
  pivot_longer(cols=!(east|year),names_to = "var",values_to = "val") %>%
  haven::write_dta(here("smalldata","B1_aggregates.dta"))



