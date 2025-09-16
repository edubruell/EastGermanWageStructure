library("pacman")
p_load(tidyverse, 
       haven,
       labelled,
       readxl, 
       glue, 
       here,
       wiesbaden)

source(here("general_functions.R"))


#---------------------------------------------------------------
# 1. What's in KLEMS?
#---------------------------------------------------------------

here("smalldata","DE_national accounts.xlsx") |> 
  readxl::excel_sheets()
#EMPE	Number of employees, th.	
#VA_PI	GVA, price indexes (2020)

here("smalldata","DE_capital accounts.xlsx") |> 
  readxl::excel_sheets()
# Capital stock net, current replacement stock, millions of national currency
#K_IT	Computing equipment capital stock 
#K_CT	Communications equipment capital stock
#K_Soft_DB	Computer software and databases capital stock


#---------------------------------------------------------------
# 2. Get the raw data from Excel
#---------------------------------------------------------------

tbl_emp <- here("smalldata","DE_national accounts.xlsx") |> 
  read_excel("EMPE") |>
  pivot_longer(cols=starts_with("19")|starts_with("20"),names_to = "year",values_to="employment") |>
  mutate(year=parse_number(year)) |>
  transmute(nace_r2_code,
            year,
            employment = employment * 1000)


tbl_price_indices <- here("smalldata","DE_national accounts.xlsx") |> 
  read_excel("VA_PI") |>
  pivot_longer(cols=starts_with("19")|starts_with("20"),names_to = "year",values_to="price_index") |>
  mutate(year=parse_number(year)) |>
  transmute(nace_r2_code,
            year,
            price_index = price_index / 100) #Make it range from 0 to 1

#Base_year four our price indices is 2020
tbl_price_indices |>
  filter(price_index==1) |>
  distinct(year)

tbl_cap_real <- c("IT","CT","Soft_DB") |>
  map(~{
    var_cap_q <- paste0("Kq_", .x)   # capital stock, chain-linked volumes (ref year)
    read_excel(here("smalldata","DE_capital accounts.xlsx"), sheet = var_cap_q) |>
      pivot_longer(cols = starts_with("19") | starts_with("20"),
                   names_to = "year", values_to = var_cap_q) |>
      mutate(year = readr::parse_number(year)) |>
      transmute(nace_r2_code, year, "{var_cap_q}" := .data[[var_cap_q]] * 1e6)
  }) |>
  reduce(left_join, by = c("nace_r2_code","year")) |>
  mutate(
    Kq_ICT      = Kq_IT + Kq_CT,
    Kq_ICT_Soft = Kq_ICT + Kq_Soft_DB
  )

tbl_inv_real <- c("IT","CT","Soft_DB") |>
  map(~{
    var_cap_q <- paste0("Iq_", .x)   # capital stock, chain-linked volumes (ref year)
    read_excel(here("smalldata","DE_capital accounts.xlsx"), sheet = var_cap_q) |>
      pivot_longer(cols = starts_with("19") | starts_with("20"),
                   names_to = "year", values_to = var_cap_q) |>
      mutate(year = readr::parse_number(year)) |>
      transmute(nace_r2_code, year, "{var_cap_q}" := .data[[var_cap_q]] * 1e6)
  }) |>
  reduce(left_join, by = c("nace_r2_code","year")) |>
  mutate(
    Iq_ICT      = Iq_IT + Iq_CT,
    Iq_ICT_Soft = Iq_ICT + Iq_Soft_DB
  )
    
#---------------------------------------------------------------
# 3. Build a tidy, per-capita capital panel (nominal & real)
#---------------------------------------------------------------

tbl_cap_export <- tbl_cap_real |>
  left_join(tbl_emp, by = c("nace_r2_code","year")) |>
  pivot_longer(cols = starts_with("Kq_"),
               names_to = "asset", values_to = "real_K") |>
  mutate(real_K_pc = real_K / employment,
         asset = stringr::str_remove(asset, "Kq_")) |>
  pivot_wider(id_cols = c(nace_r2_code, year, employment),
              names_from = asset,
              values_from = c(real_K, real_K_pc),
              names_sep = "_")

tbl_inv_export <- tbl_inv_real |>
  left_join(tbl_emp, by = c("nace_r2_code","year")) |>
  pivot_longer(cols = starts_with("Iq_"),
               names_to = "asset", values_to = "real_I") |>
  mutate(real_I_pc = real_I / employment,
         asset = stringr::str_remove(asset, "Iq_")) |>
  pivot_wider(id_cols = c(nace_r2_code, year, employment),
              names_from = asset,
              values_from = c(real_I, real_I_pc),
              names_sep = "_")


# Add variable labels and export to Stata
tbl_cap_export |>
  set_variable_labels(
    nace_r2_code      = "NACE Rev. 2 industry code",
    year              = "Calendar year",
    employment        = "Number of employees (persons)",
    
    # Capital stocks (chain-linked volumes, reference year 2020)
    real_K_IT         = "Computing equipment capital stock (chain-linked volume, 2020 reference year)",
    real_K_CT         = "Communications equipment capital stock (chain-linked volume, 2020 reference year)",
    real_K_Soft_DB    = "Software & databases capital stock (chain-linked volume, 2020 reference year)",
    real_K_ICT        = "ICT capital stock (chain-linked volume, 2020 reference year)",
    real_K_ICT_Soft   = "ICT + Software/DB capital stock (chain-linked volume, 2020 reference year)",
    
    # Per-worker measures
    real_K_pc_IT      = "Computing equipment capital stock per worker (chain-linked volume, 2020 ref.)",
    real_K_pc_CT      = "Communications equipment capital stock per worker (chain-linked volume, 2020 ref.)",
    real_K_pc_Soft_DB = "Software & databases capital stock per worker (chain-linked volume, 2020 ref.)",
    real_K_pc_ICT     = "ICT capital stock per worker (chain-linked volume, 2020 ref.)",
    real_K_pc_ICT_Soft= "ICT + Software/DB capital stock per worker (chain-linked volume, 2020 ref.)"
  ) |>
  write_dta(here("smalldata", "KLEMS_IT_capital.dta"))

# Add variable labels and export to Stata
tbl_inv_export |>
  set_variable_labels(
    nace_r2_code      = "NACE Rev. 2 industry code",
    year              = "Calendar year",
    employment        = "Number of employees (persons)",
    
    # Capital stocks (chain-linked volumes, reference year 2020)
    real_I_IT         = "Computing equipment investments (chain-linked volume, 2020 reference year)",
    real_I_CT         = "Communications equipment investments (chain-linked volume, 2020 reference year)",
    real_I_Soft_DB    = "Software & databases investments (chain-linked volume, 2020 reference year)",
    real_I_ICT        = "ICT investments (chain-linked volume, 2020 reference year)",
    real_I_ICT_Soft   = "ICT + Software/DB investments (chain-linked volume, 2020 reference year)",
    
    # Per-worker measures
    real_I_pc_IT      = "Computing equipment investments per worker (chain-linked volume, 2020 ref.)",
    real_I_pc_CT      = "Communications equipment investments per worker (chain-linked volume, 2020 ref.)",
    real_I_pc_Soft_DB = "Software & databases investments per worker (chain-linked volume, 2020 ref.)",
    real_I_pc_ICT     = "ICT investments per worker (chain-linked volume, 2020 ref.)",
    real_I_pc_ICT_Soft= "ICT + Software/DB investments per worker (chain-linked volume, 2020 ref.)"
  ) |>
  write_dta(here("smalldata", "KLEMS_IT_investments.dta"))


#---------------------------------------------------------------
# 4. Create East/West Measure using Bruttoanlageinvestitionen
#---------------------------------------------------------------

tbl_inv_series_destatis <- retrieve_datalist(tableseries = "82*",
                                             genesis = regdb_login)

retrieve_metadata("82000LJ009",genesis = regdb_login)
retrieve_metadata("82000LJ010",genesis = regdb_login)

tbl_anlage_inv_raw <- retrieve_data("82000LJ010",genesis = regdb_login)


wz24vgru_to_nace <- tribble(
  ~WZ24VGRU,   ~nace_r2_code,
  
  # Single-section groups
  "WZ08-C",    "C",   # Verarbeitendes Gewerbe
  "WZ08-J",    "J",   # Information & Kommunikation
  "WZ08-K",    "K",   # Finanz- und Versicherungsdienstleistungen
  "WZ08-L",    "L",   # Grundstücks- und Wohnungswesen
  
  # Bundles expanded to multiple NACE sections
  "WZ08-G-01", "G",   # Handel, Gastgewerbe, Verkehr (G–I)
  "WZ08-G-01", "H",
  "WZ08-G-01", "I",
  
  "WZ08-M-N",  "M",   # Unternehmensdienstleistungen (M–N)
  "WZ08-M-N",  "N",
  
  "WZ08-O-Q",  "O",   # Öffentliche DL, Erziehung, Gesundheit (O–Q)
  "WZ08-O-Q",  "P",
  "WZ08-O-Q",  "Q",
  
  "WZ08-R-T",  "R",   # Sonstige DL, private HH (R–T)
  "WZ08-R-T",  "S",
  "WZ08-R-T",  "T"
)

tbl_inv_shares_east_west <- tbl_anlage_inv_raw |>
  group_by(east = as.integer(DLAND >=11),
           WZ24VGRU, 
           year = JAHR) |>
  summarise(brutto_anlage_inv = sum(VGR24BA2_val,na.rm=TRUE),
            .groups="drop") |>
  mutate(east = if_else(east == 1L, "east", "west")) |>
  pivot_wider(names_from = east, values_from = brutto_anlage_inv, values_fill = 0) |>
  mutate(
    total = east + west,
    share_east = if_else(total > 0, east / total, NA_real_),
    share_west = if_else(total > 0, west / total, NA_real_)
  ) |>
  print(n=Inf)

tbl_inv_east_west <- wz24vgru_to_nace |>
  expand_grid(year=1995:2021) |>
  left_join(tbl_inv_shares_east_west,by = join_by(WZ24VGRU, year)) |>
  group_by(nace_r2_code) |>
  arrange(year, .by_group = TRUE) |>
  tidyr::fill(share_east, share_west, east, west, total, .direction = "up") |>
  ungroup() |>
  select(nace_r2_code,year,starts_with("share")) |>
  pivot_longer(cols = starts_with("share"),
               names_to = "east",
               values_to = "share",
               names_prefix = "share_") |>
  left_join(
    read_dta(here("smalldata", "KLEMS_IT_investments.dta")) |>
      filter(!nace_r2_code %in% c("TOT", "TOT_IND", "MARKT", "MARKTxAG"),
             !str_detect(nace_r2_code, "-"),
             !str_detect(nace_r2_code, "\\d+")
      ) |>
      select(-contains("_pc_")),by=c("nace_r2_code","year")
  ) |>
  mutate(across(starts_with("real_I"), ~{share*.x})) |>
  select(nace_r2_code,year,east,starts_with("real_"))


retrieve_metadata("13312LJ001",genesis = regdb_login)

tbl_employment_raw <- retrieve_data("13312LJ001",genesis = regdb_login)


tbl_employment_shares_east_west <- tbl_employment_raw |>
  group_by(east = as.integer(DLAND >= 11),
           WZ08SE, 
           year = JAHR) |>
  summarise(employment = sum(ERW063_val, na.rm = TRUE),
            .groups = "drop") |>
  mutate(east = if_else(east == 1L, "east", "west")) |>
  pivot_wider(names_from = east, values_from = employment, values_fill = 0) |>
  mutate(
    total = east + west,
    share_east = if_else(total > 0, east / total, NA_real_),
    share_west = if_else(total > 0, west / total, NA_real_)
  )


wz08se_to_nace <- tribble(
  ~WZ08SE,      ~nace_r2_code,
  
  "WZ08-A",     "A",    # Land- und Forstwirtschaft, Fischerei
  "WZ08-B-18",  "B",    # Bergbau und Gewinnung von Steinen und Erden
  "WZ08-C",     "C",    # Verarbeitendes Gewerbe
  "WZ08-F",     "F",    # Baugewerbe
  
  "WZ08-G-01-I-J", "G", # Handel, Gastgewerbe, Verkehr, Information
  "WZ08-G-01-I-J", "H",
  "WZ08-G-01-I-J", "I", 
  "WZ08-G-01-I-J", "J",
  
  "WZ08-KL",    "K",    # Finanz- und Versicherungsdienstleistungen, Grundstücks- und Wohnungswesen
  "WZ08-KL",    "L",
  
  "WZ08-PX",    "M",    # Sonstige Dienstleistungen (M-U)
  "WZ08-PX",    "N",
  "WZ08-PX",    "O",
  "WZ08-PX",    "P",
  "WZ08-PX",    "Q",
  "WZ08-PX",    "R",
  "WZ08-PX",    "S",
  "WZ08-PX",    "T",
  "WZ08-PX",    "U"
)

klems_industry_emp <- wz08se_to_nace |>
  expand_grid(year=1995:2021) |>
  left_join(tbl_employment_shares_east_west,by = join_by(WZ08SE, year)) |>
  group_by(nace_r2_code) |>
  arrange(year, .by_group = TRUE) |>
  tidyr::fill(share_east, share_west, east, west, total, .direction = "up") |>
  ungroup() |>
  select(nace_r2_code,year,starts_with("share"))  |>
  pivot_longer(cols = starts_with("share"),
               names_to = "east",
               values_to = "share",
               names_prefix = "share_") |>
left_join(
  read_dta(here("smalldata", "KLEMS_IT_investments.dta")) |>
    filter(!nace_r2_code %in% c("TOT", "TOT_IND", "MARKT", "MARKTxAG"),
           !str_detect(nace_r2_code, "-"),
           !str_detect(nace_r2_code, "\\d+")
    ) |>
    select(nace_r2_code,year,employment),by=c("nace_r2_code","year")
) |>
  mutate(employment = employment * share) |>
  select(nace_r2_code,year,east,employment)


tbl_inv_east_west |>
  left_join(klems_industry_emp,by=c("nace_r2_code","year","east")) |>
  write_dta(here("smalldata", "KLEMS_IT_investments_ew.dta"))

#---------------------------------------------------------------
# 5. Visualize sectoral trends in KLEMS capital
#---------------------------------------------------------------

klems_sector <- read_dta(here("smalldata", "KLEMS_IT_investments_ew.dta")) |>
  # drop totals and aggregate categories
  mutate(sector = case_when(
    nace_r2_code %in% c("A", "B") ~ "Agriculture/Mining",
    nace_r2_code == "C"           ~ "Manufacturing",
    nace_r2_code == "F"           ~ "Construction",
    TRUE                          ~ "Services"
  )) |>
  group_by(year, sector,east) |>
  summarise(across(!contains("_pc_") & starts_with("real_I") | employment,
                   ~sum(.x, na.rm = TRUE)), .groups = "drop") |>
  mutate(across(starts_with("I_")| starts_with("real_I"),
                ~.x/employment
                )
         )


klems_labels <- labelled::get_variable_labels(read_dta(here("smalldata", "KLEMS_IT_capital.dta")))


plot_KLEMS_sector_series <- function(variable){
  klems_sector |>
    ggplot() +
    aes(x = year, y = .data[[variable]], color = sector) +
    geom_line(linewidth = 1.25) +
    scale_color_brewer(palette = "Set1") +
    scale_y_continuous(labels = scales::label_dollar(suffix="EUR p.c.",prefix=""))+
    labs(
      x = "Year",
      y = klems_labels[[variable]],
      color = "Sector"
    ) +
    theme_bw(base_size = 20) +
    facet_wrap(~east)
}

plot_KLEMS_sector_series("real_I_ICT")

#Mit bruttoanlageinverstitionen Investments zwischen Ost-West skalieren
#KLEMMS Investments aufteilen auf Ost/West
# Ab 2000 -> rück extraplolieren

read_dta(here("smalldata", "KLEMS_IT_investments_ew.dta")) |>
  mutate(across(starts_with("I_")| starts_with("real_I"),
                ~.x/employment
  )
  ) |>
  ggplot() +
  aes(x = year, y = .data[["real_I_ICT"]], color = nace_r2_code) +
  geom_line(linewidth = 1.25) +
  scale_color_viridis_d() +
  scale_y_continuous(labels = scales::label_dollar(suffix="EUR p.c.",prefix=""))+
  labs(
    x = "Year",
    y = klems_labels[["real_I_ICT"]],
    color = "Industry"
  ) +
  theme_bw(base_size = 20) +
  facet_wrap(~east) 
