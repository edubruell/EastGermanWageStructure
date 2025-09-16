
#Factor names 
fct_educ <- tribble(
  ~faclevel, ~label,
  1,"Low Education",
  2,"Medium Education",
  3,"High Education",
  9999,"Missing Education Info"
)

fct_sub40 <- tribble(
  ~faclevel, ~label,
  0,"Over 40 Year-Olds",
  1,"Under 40 Year-Olds",
  9999,"Missing Age Info")

fct_age_educ <- tribble(
  ~faclevel, ~label,
  1,"Over 40  - Low Education",
  2,"Over 40  - Medium Education",
  3,"Over 40  - High Education",
  4,"Under 40  - Low Education",
  5,"Under 40  - Medium Education",
  6,"Under 40  - High Education")

fct_sector <- tribble(
  ~faclevel, ~label,
  1,"Agriculture/Mining",
  2,"Manufacturing",
  3,"Construction",
  4,"Services",
  9999,"Missing Industry")

fct_svbroad <- tribble(
  ~faclevel, ~label,
  1,"Agriculture/Mining",
  2,"Manufacturing",
  3,"Construction",
  4,"Public Administration and Education",
  5,"Services (Private)")

fct_industry <- tribble(
  ~faclevel, ~label,
  1,"Agriculture/Mining",
  2,"Manufacturing (consumables)",
  3,"Manufacturing (chemistry/metal)",
  4,"Manufacturing (mechanical)",
  5,"Construction",
  6,"Retail",
  7,"Transportation",
  8,"Hospitality",
  9,"Business services",
  10,"Financial services",
  11,"Public administration",
  12,"Education",
  13,"Health Care",
  14,"Other services",
  9999,"Missing Industry Information")


fct_firm_fe <- tribble(
  ~faclevel, ~label,
  1,"1. Quartile",
  2,"2. Quartile",
  3,"3. Quartile",
  4,"4. Quartile", 
  9999,"Missing Firm Effect")

fct_tagw <- tribble(
  ~faclevel, ~label,
  0,"All other industries",
  1,"Temporary agency work",
  9999,"Missing Industry Information"
)


fct_industry <- tribble(
  ~faclevel, ~label,
  1,"Agriculture/Mining",
  2,"Manufacturing (consumables)",
  3,"Manufacturing (chemistry/metal)",
  4,"Manufacturing (mechanical)",
  5,"Construction",
  6,"Retail",
  7,"Transportation",
  8,"Hospitality",
  9,"Business services",
  10,"Financial services",
  11,"Public administration",
  12,"Education",
  13,"Health Care",
  14,"Other services",
  9999,"Missing Industry Information")


fct_sector <- tribble(
  ~faclevel, ~label,
  1,"Agriculture/Mining",
  2,"Manufacturing",
  3,"Construction",
  4,"Services",
  9999,"Missing Industry")



fct_sec_educ <- expand_grid(e = 1:3,s = 1:4) %>%
  mutate(faclevel = 1:n(),
         e = case_when(e == 1 ~ "Low Educ.",
                              e == 2 ~ "Mid. Educ.",
                              e == 3 ~ "High Educ."),
         s = case_when(s == 1 ~ "Agriculture/Mining",
                       s == 2 ~ "Manufacturing",
                       s == 3 ~ "Construction",
                       s == 4 ~ "Services"),
         label = glue("{s} - {e}")) %>%
  select(faclevel, label) 
  

fct_fctrl <- fct_industry %>%
  filter(faclevel <=14) %>%
  rename(i = faclevel) %>%
  expand_grid(a = c(0,1),
              e = c(1,2,3))  %>%
  arrange(e,a,i) %>% 
  mutate(faclevel = 1:n(),
         a = if_else(a==0,"Over 40","Sub 40"),
         e = case_when(   e == 1 ~ "Low Educ.",
                          e == 2 ~ "Mid. Educ.",
                          e == 3 ~ "High Educ."),
         label = glue("{e} - {a} - {label}")) %>%
  select(faclevel, label) 


fct_occ2 <- read_dta(here("smalldata","occ2_classifciation.dta")) %>%
  select(-beruf_2010gr) %>%
  distinct() %>%
  rename(faclevel = occ2, label = occ2_name) %>%
  add_row(faclevel = 9999, label = "Missing Occupation")

fct_q_firm <- tribble(
  ~faclevel, ~label,
  1,"1. Quartile",
  2,"2. Quartile",
  3,"3. Quartile",
  4,"4. Quartile", 
  9999,"Missing Firm Effect")

#Firm Median and mean wages get the same label
fct_q_firm_med <- fct_q_firm
fct_q_firm_mw  <- fct_q_firm


fct_t_firm <- tribble(
  ~faclevel, ~label,
  1,"1. Tercile",
  2,"2. Tercile",
  3,"3. Tercile",
  9999,"Missing Firm Effect")

fct_o_firm <- fct_t_firm

fct_lw2008 <- tribble(
  ~faclevel, ~label,
  0  , "Alle anderen",
  1  , "Abfallwirtschaft (2010 to 2014)",
  2  , "Leiharbeit / Zeitarbeit (2012 to 2014)",
  3  , "Sicherheitsdienste (2012 and 2013)",
  4  , "Gebäudereinigung  (2008 to 2014)",
  5  , "Erwachsenenbildung (2013/2014)",
  6  , "Pflege	(2011 to 2014)",
  7  , "Wäscherei (2010 to 2014)",
  8  , "Bauhauptgewerbe",
  9  , "Elektroinstalation",
  10 , "Maler",
  11 , "Dachdecker",
  12 , "Gerüstbauer")


fct_firm_size <- tribble(
  ~faclevel, ~label,
   1 ,"1-10 Employees",
   2 ,"10-25 Employees",
   3 ,"25-100 Employees",
   4 ,"100-500 Employees",
   5 ,"More than 500 Employees")

fct_lw_ind <- tribble(
  ~faclevel, ~label,
  0, "All other industries",
  1, "Temporary Agency work",
  2, "Security services",
  3, "cleaning of buildings, etc.",
  4, "Other professional services", 
  5, "Health care services",
  6, "social work",
  7, "Refuse disposal",
  8, "Construction")

fct_temp_agency<- tribble(
  ~faclevel, ~label,
  0, "All other industries",
  1, "Temporary Agency work")

fct_tw_fe <-expand_grid(q_firm = 1:4,
            q_pers = 1:4) %>%
  mutate(faclevel = 1:n(),
         label    = glue("{q_firm}. Quartile Firm FE - {q_pers}. Quartile Person FE"))  %>%
  select(faclevel, label) 

fct_serv_int <-expand_grid(serv_fe = c("Below Median Firm FE","Above Median Firm FE"),
                           d_serv  = c("Other Industries","Services")) %>%
  mutate(faclevel = 1:n(),
         label    = glue("{d_serv} - {serv_fe}")) %>%
  select(faclevel, label) 

fct_tarif <- tribble(
  ~faclevel, ~label,
  1,"Industry union agreement",
  2, "Firm union agreement",
  3, "No union agreement",
  9999, "Missing union information")


fct_fs_quant <-tribble(
  ~faclevel, ~label,
  1, "1. Qrt. 1   -  25 Employees",
  2, "2. Qrt. 25  - 107 Employees",
  3, "3. Qrt. 107 - 469 Employees",
  4, "4. Qrt. More than 469 Employees",
  9999, "Missing firm-sze information"
)

fct_ind2d <- read_xlsx(here("smalldata","wz93_2_names.xlsx")) %>%
  transmute(faclevel=wz93_2,label=wz93_2_lab)


fct_d_services <-tribble(
  ~faclevel, ~label,
  0, "Other industries",
  1, "Service industries"
)


fct_sector_d <-tribble(
  ~faclevel, ~label,
  1,"Agriculture/Mining",
  2,"Manufacturing",
  3,"Construction",
  4,"Services",
  5,"Utilites"
)




# Definitions for factor collapsing start here
#=========================================================

fcol_occ2_occ1 <- tribble(~ocode, ~nfacname, ~nlevel, 
        1,  "Agriculture/Gardening" ,                1, 
        2,  "Agriculture/Gardening" ,                1, 
        3,  "Intermediate Good Mft",                 2, 
        4,  "Intermediate Good Mft",                 2, 
        5,  "Intermediate Good Mft",                 2, 
        6,  "Metal, Machinery, Automotive",          3, 
        7,  "Metal, Machinery, Automotive",          3, 
        8,  "Metal, Machinery, Automotive",          3, 
        9,  "Research, Development and Science",     4, 
        10,  "Intermediate Good Mft",                 2, 
        11,  "Food-production",                       5, 
        12,  "Architecture",                          6, 
        13,  "Construction Occupations",              7, 
        14,  "Construction Occupations",              7, 
        15,  "Waste and Cleaning",                    8, 
        16,  "Research, Development and Science",     4, 
        17,  "Research, Development and Science",     4, 
        18,  "IT Occupations",                        9, 
        19,  "Traffic and logistics",                 10,
        20,  "Traffic and logistics",                 10,
        21,  "Low Skill Services",                    11,
        22,  "Low Skill Services",                    11,
        23,  "Low Skill Services",                    11,
        24,  "Low Skill Services",                    11,
        25,  "Hospitality Occupations",               12,
        26,  "Management and Accounting",             13,
        27,  "Management and Accounting",             13,
        28,  "Public Admin",                          14,
        29,  "Health care",                           15,
        30,  "Health care",                           15,
        31,  "Education",                             16,
        32,  "Education",                             16,
        33,  "Research, Development and Science",     4, 
        34,  "Other occupations ",                    17,
        35,  "Other occupations ",                    17,
        36,  "Other occupations ",                    17,
        37,  "Other occupations ",                    17,
        38,  "Missing Occupation",   				9999,
)

fcol_ind2d_ind1d <- read_xlsx(here("smalldata","wz93_ctable_2_to_1.xlsx")) %>%
  transmute(ocode    = wz93_2,
            nfacname = wz93_1_lab, 
            nlevel   = factor(wz93_1) %>% as.integer())

fcol_fctrl_educt <- fct_industry %>%
  filter(faclevel <=14) %>%
  rename(i = faclevel) %>%
  expand_grid(a = c(0,1),
              e = c(1,2,3))  %>%
  arrange(e,a,i) %>% 
  mutate(ocode = 1:n(),
         nfacname = case_when(   e == 1 ~ "Low Education",
                                 e == 2 ~ "Medium Education",
                                 e == 3 ~ "High Education"),
         nlevel = e) %>%
  select(ocode, nfacname, nlevel) 



fcol_tw_fe_q_firm <-expand_grid(nlevel = 1:4,
                        q_pers = 1:4) %>%
  mutate(ocode = 1:n(),
         nfacname    = glue("{nlevel}. Firm FE"))  %>%
  select(ocode, nfacname, nlevel)

