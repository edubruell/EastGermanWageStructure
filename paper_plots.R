library("pacman")
p_load(tidyverse, 
       haven, 
       readxl, 
       scales, 
       stringr, 
       glue, 
       here, 
       gghighlight,
       patchwork)

source(here("functions","general_functions.R"))
source(here("functions","factor_definitions.R"))
source(here("functions","graphing_functions.R"))


#Read Info from stata outputs
log_main  <- read_st_matrices(here("logs","eb01_overview.log"))
log_liab_unions   <- read_st_matrices(here("logs","eb08_unions_rif.log")) #LIAB results



#====================================================================
#   1. Prepare data from logs for plots
#====================================================================

# 1a. Prepare plot/wgap_contribution data for RIF decompositions
#----------------------------------------------------------------

#Preprocess union RIFs from LIAB (different output format)
union_rifs<- log_liab_unions[c(grep("s_",names(log_liab_unions)),grep("r_",names(log_liab_unions)))] %>%
  map(~{.x %>%
      filter(ffl!="0") %>%
      mutate(ffl = parse_number(ffl)-1,
             ffl = as.character(ffl))})


#RIF Percentile and WGAP matrices output from otf_ffl command
log_s <- log_main[grep("^s_", names(log_main))]   %>%
  c(union_rifs[grep("^s_",names(union_rifs))]) %>%
  set_names(gsub("^s_","",names(.)))

log_r <- log_main[grep("^r_", names(log_main))] %>%
  c(union_rifs[grep("^r_",names(union_rifs))]) %>%
  set_names(gsub("^r_","",names(.)))

rif_names <- names(log_s)

#Prepare data routine for RIFs
prepare_data <- function(.name){
  map(rif_names,merge_mat) %>%
    map(~parse_tbl(.,.name)) |> 
    map(convert_factors)  |>
    set_names(rif_names) 
}

c("pct","wgap") %>% 
  map_assign(prepare_data,.names=c("rif_pct","rif_wgap"))

# 1b. Prepare plot/wgap_contribution data for DFL decompositions
#----------------------------------------------------------------

#DFL Decompoostion output
raw_pct_dfl <- log_main[grep("p_dfl",names(log_main))] 

#Map to single tibble with name column
pct_dfl <- map2_dfr(.x=raw_pct_dfl, 
                  .y=names(raw_pct_dfl), 
                  .f=function(x,y){x %>% 
                      mutate(var = gsub("m_p_dfl_","",y))
                  }) %>% 
  select(-rownum) %>%
  pivot_longer(cols         = starts_with("p"),
               names_to     = "pct",
               names_prefix = "p",
               values_to    = "dfl_p") %>%
  mutate(across(c("east","year","dfl_p"),parse_number),
         east = if_else(east==1, "East","West"),
         pct  = parse_integer(pct)) |>
  select(-starts_with("n")) |>
  mutate(var=if_else(var=="demo","age_educ",var))

pct_act <- log_main$m_pct |>
  mutate(across(everything(),parse_number),
         east = if_else(east==1, "East","West")) |>
  pivot_longer(cols         = starts_with("p"),
               names_to     = "pct",
               names_prefix = "p",
               values_to    = "p") |>
  mutate(pct  = parse_integer(pct))

# 1c. Prepare comp_actual needed for RIF outputs
#----------------------------------------------------------------

comp_actual_pct <- log_main$m_pct |>
  mutate(across(everything(),parse_number),
         east = if_else(east==1, "East","West")) |>
  pivot_longer(cols         = starts_with("p"),
               names_to     = "pct",
               names_prefix = "p",
               values_to    = "p") |>
  mutate(pct  = parse_integer(pct)) |>
  select(-starts_with("n")) |>
  left_join(pct_dfl, by=c("pct","east","year")) |> 
  select(pct,east,year,var, p, dfl_p)

  
comp_actual_wgap <- comp_actual_pct %>%
  pivot_wider(id_cols     = c("east","year","var"),
              names_from  = "pct",
              values_from = c("p","dfl_p")) %>%
  mutate(w_85_50.act = log(p_85/p_50),
         w_50_15.act = log(p_50/p_15),
         w_85_50.dfl = log(dfl_p_85/dfl_p_50),
         w_50_15.dfl = log(dfl_p_50/dfl_p_15)) %>%
  select(east,year,var,starts_with("w")) %>%
  pivot_longer(starts_with("w"),names_to = "wgraw", names_prefix = "w_")  %>%
  separate(wgraw,c("wgap","dfl"),sep="\\.") %>%
  mutate(wgap = str_replace(wgap,"_","-")) %>%
  pivot_wider(id_cols     = c("east","year","var","wgap"),
              values_from = "value", 
              names_from  = "dfl")%>%
  rename(w = act, w_dfl = dfl) 

#====================================================================
#   2. Exhibits for the stylized facts section
#====================================================================

p_wg_trend <- comp_actual_wgap |>
  filter(var=="age_educ") |>
  transmute(east,
            year,
            ineq = glue("{wgap} Wage Gap"),
            w) |>
  bind_rows(log_main$m_sd_educ  |>
              mutate(across(everything(),parse_number),
                     east = if_else(east==1, "East","West")) |>
              transmute(east,year, ineq = "Std. Deviation of Wages",w = sd)) |>
  ggplot()+
  aes(x=year,y=w,colour=ineq,shape=ineq, linetype=ineq) +
  geom_line(linewidth=1.4) +
  geom_point(size=2.6) +
  facet_wrap(~east) +
  theme_eddy(23) +
  theme(text=element_text(size=22)) +
  scale_color_brewer(palette = "Set1")+
  scale_linetype_manual(values=c("solid","solid","dashed")) +
  scale_y_continuous(n.breaks = 8)+
  labs(y= "Measures of Dispersion",
       x= "Year",
       colour ="Inequality measure",
       shape  ="Inequality measure",
       linetype = "Inequality measure") +
  guides(colour = guide_legend(title.position = "top",title.hjust=0.5,ncol=3))


p_indexed_wage_growth <- pct_act |>
  group_by(east,pct) |>
  arrange(east,pct,year) |>
  mutate(i_p = p/p[1],
        pct  = glue("{pct}th percentile")) |>
  ggplot() +
  aes(x=year,y=i_p,colour=pct,shape=pct) +
  geom_line(linewidth=1.4) +
  geom_point(size=2.6) +
  facet_wrap(~east) +
  theme_eddy(23) +
  theme(text=element_text(size=22)) +
  scale_color_brewer(palette = "Set1")+
  #scale_colour_manual(values = c(brewer_pal(palette = "Set1")(2),"#282828"))+
  scale_y_continuous(n.breaks = 8, labels=label_percent()) +
  labs(y="Indexed real wage growth",
       x="Year",
       colour="Percentile",
       shape ="Percentile") +
  guides(colour = guide_legend(title.position = "top",title.hjust=0.5,ncol=3))

ggsave(plot= p_indexed_wage_growth,
       filename = here("plots","2_stylized_facts","indexed_wage_growth.pdf"),
       width=16,
       height=9)

#====================================================================
#3. Exhibits for the supply side section
#====================================================================

# 3.1 Workforce Composition
#--------------------------

p_dfl_age_educ <- list(log_main$m_pct,log_main$m_p_dfl_demo) |>
  imap_dfr(~{
    .x |> 
      mutate(across(everything(),parse_number),
             east = if_else(east==1, "East","West")) |>
      mutate(wg85_50 = log(p85/p50),
             wg50_15 = log(p50/p15),
             dfl=.y)
  }) |>
  mutate(dfl = factor(dfl, levels=1:2, 
                      labels = c("Actual wage distribution",
                                 "Adjusted to 1995 Demographics"))) |>
  select(east,year,dfl,starts_with("wg")) |>
  pivot_longer(starts_with("wg"),names_to = "var",values_to = "value") |>
  mutate(wage_gap = if_else(str_detect(var,"85_50"),"85-50 Wage Gap","50-15 Wage Gap")) |>
  ggplot() +
  aes(y=value,
      x=year,
      colour=wage_gap,
      shape=wage_gap,
      linetype=dfl) +
  geom_line(linewidth=1.4) +
  geom_point(size=2.6) +
  facet_wrap(~east) +
  theme_eddy(23) +
  scale_color_brewer(palette = "Set1")+
  scale_linetype_manual(values=c("solid","dashed")) +
  scale_y_continuous(n.breaks = 8) +
  labs(x="Year",           
       y ="Inequality measure",
       colour ="",
       shape  ="",
       linetype="") +
  guides(colour   = guide_legend(ncol = 1),
         linetype = guide_legend(ncol = 1)) 

ggsave(here("plots","3_supply","dfl_age_educ.pdf"),p_dfl_age_educ,width=16,height=9)

# 3.2 Contributions to wage gap increases
#-----------------------------------------

d_rif_age_educ <- generate_wgap_contrib("age_educ","Age X Education Composition") %>%
  mutate(educ = case_when(str_detect(age_educ,"Low")    ~ 1L,
                          str_detect(age_educ,"Medium") ~ 2L,
                          str_detect(age_educ,"High")   ~ 3L,
                          str_detect(age_educ,"Composition") ~ 4L),
         educ = factor(educ,labels=c("Low Education","Medium Education","High Education","Age X Education Composition")),
         wgap = factor(wgap,levels=c("85-50","50-15"),labels=c("85-50 Wage Gap","50-15 Wage Gap"))) %>%
  group_by(wgap,east,year,educ) %>%
  summarise(contrib =sum(contrib)*100,.groups = "drop") 

p_wgap_contrib_age_educ <- d_rif_age_educ %>%
  plot_rif_contrib(.y    = "contrib",
                   .gvar = "educ",
                   .ylab = "Within-education group contribution\nto overall changes of inequality",
                   .spal = c(15,16,17,18))

ggsave(here("plots","3_supply","rif_contrib_wgap_age_educ.pdf"),p_wgap_contrib_age_educ,width=16,height=16)

# 3.3 Changes along the Employment Margin
#-----------------------------------------


#Post 2020 we only start in 2024
read_csv2(here("smalldata","12211-1003_de_flat.csv")) |>
  filter(`2_variable_attribute_label`=="Insgesamt",
         `3_variable_attribute_label`=="Insgesamt") |>
  filter(value_variable_label  %in% c("Bevölkerung in Hauptwohnsitzhaushalten","Erwerbstätige aus Hauptwohnsitzhaushalten")) |>
  transmute(bula = parse_number(`1_variable_attribute_code`),
            year = time,
            east = if_else(bula>=1 & bula<=10, FALSE,TRUE),
            value = parse_number(value)*1000,
            variable = if_else(value_variable_label=="Erwerbstätige aus Hauptwohnsitzhaushalten","emp","pop")) |>
  pivot_wider(names_from = "variable", values_from = "value") |>
  arrange(bula,year)
            
#Get the data from destatis
d_popemp <- read_csv2(here("smalldata","12211-9004_de_flat.csv")) |>
  filter(value_variable_label %in% c("Erwerbstätige","Bevölkerung")) |>
  transmute(bula = parse_number(`1_variable_attribute_code`),
            year = if_else(time_label=="Jahr", 
                           parse_number(time),
                           str_extract(time,"[^\\/]*$") %>% 
                             parse_number()),
            east = if_else(bula>=1 & bula<=10, FALSE,TRUE),
            value = value*1000,
            variable = if_else(value_variable_label=="Erwerbstätige","emp","pop")
            ) |>
  pivot_wider(names_from = "variable", values_from = "value") |>
  arrange(bula,year)


#East/west data
d_ew_popemp <- d_popemp |>
  group_by(east,year) |>
  summarise(across(emp|pop, sum),.groups="drop") |>
  filter(year>=1995) |>
  arrange(east,year) |>
  group_by(east) |>
  mutate(across(emp|pop, ~.x/first(.x))) |>
  pivot_longer(emp|pop,
               names_to = "variable",
               values_to = "change") %>%
  mutate(variable = if_else(variable=="emp","Employment","Population"),
         east     = if_else(east, "East","West"))


p_emp_change <- d_ew_popemp %>%
  mutate(variable = factor(variable,levels= c("Population","Employment"))) %>%
  ggplot() +
  aes(x        = year,
      y        = change,
      colour   = east,
      linetype = variable,
      shape    = east) +
  geom_line(linewidth=1.4) +
  geom_point(size=2.6) +
  scale_y_continuous(label=scales::percent_format()) +
  scale_x_continuous(limits=c(1995,2021),n.breaks = 8) +
  scale_color_brewer(palette="Set1") +
  theme_eddy(23) +
  labs(x="Year",y="Change relative to 1995",colour="",linetype="", shape="") + 
  theme(legend.key.width = unit(3, "line"))

ggsave(here("plots","3_supply","emp_population_ew.pdf"),p_emp_change,width=16,height=9)


read_csv2(here("smalldata","internal_mig_net.csv"),
          col_names = c("year","to_from_wg","total"),
          skip=1) %>%
  mutate(to_from_berlin = total - to_from_wg) %>%
  filter(year>=1995) %>%
  mutate(across(!year,~{.*1000L})) %>%
  pivot_longer(cols = !year, names_to = "variable", values_to = "value",names_prefix = "r_") %>%
  mutate(variable =  case_when(variable == "to_from_wg" ~ "To/From West Germany",
                               variable == "total" ~ "Total",
                               variable == "to_from_berlin" ~ "To/From Berlin"),
         ftitle   = "Internal migration") %>%
  ggplot(aes(x=year,y=value,colour=variable, shape=variable,linetype=variable)) +
  geom_line(size=1.4) +
  geom_point(size=2.6) +
  scale_x_continuous(limits=c(1995,2017),n.breaks = 6) +
  scale_y_continuous(label = label_number(),n.breaks = 6)+
  scale_color_manual(values=c(brewer_pal(palette = "Set1")(2),"black")) +
  scale_linetype_manual(values = c("solid","solid","dashed")) +
  theme_eddy(23) +
  theme(strip.text.x = element_text(debug=FALSE,size = rel(0.8),margin = margin(3,0,3,0, "pt")),
        legend.key.width = unit(3, "line")) +
  labs(y="",x="Year",colour="",shape="",linetype="") +
  facet_wrap(~ftitle) +
  guides(colour=guide_legend(ncol = 1))


p_internal_mig <- read_dta(here("smalldata","B1_internal_migration.dta")) %>%
  pivot_longer(cols = !year, names_to = "variable", values_to = "value") %>%
  mutate(variable =  case_when(variable == "to_from_wg" ~ "To/From West Germany",
                               variable == "total" ~ "Total",
                               variable == "to_from_berlin" ~ "To/From Berlin"),
         ftitle   = "Internal migration") %>%
  ggplot(aes(x=year,y=value,colour=variable, shape=variable,linetype=variable)) +
  geom_line(linewidth=1.4) +
  geom_point(size=2.6) +
  scale_x_continuous(limits=c(1995,2017),n.breaks = 6) +
  scale_y_continuous(label = label_number(),n.breaks = 6)+
  scale_color_manual(values=c(brewer_pal(palette = "Set1")(2),"black")) +
  scale_linetype_manual(values = c("solid","solid","dashed")) +
  theme_eddy(23) +
  theme(strip.text.x = element_text(debug=FALSE,size = rel(0.8),margin = margin(3,0,3,0, "pt")),
        legend.key.width = unit(3, "line")) +
  labs(y="",x="Year",colour="",shape="",linetype="") +
  facet_wrap(~ftitle) +
  guides(colour=guide_legend(ncol = 1))



p_international_mig <-   read_dta(here("smalldata","B1_international_migration.dta")) %>%
  pivot_longer(cols = !year, names_to = "variable", values_to = "value") %>%
  mutate(variable =  case_when(variable == "to_west"   ~ "To West Germany",
                               variable == "to_east"   ~ "To East Germany",
                               variable == "to_berlin" ~ "To Berlin",
                               variable == "total"     ~ "Total international migration"),
         ftitle  = "Net international migration") %>%
  ggplot(aes(x=year,y=value,colour=variable, shape=variable,linetype=variable)) +
  geom_line(linewidth=1.4) +
  geom_point(size=2.6) +
  scale_x_continuous(limits=c(1995,2015),n.breaks = 6) +
  scale_y_continuous(label = label_number(),n.breaks = 6)+
  scale_color_manual(values=c(brewer_pal(palette = "Set1")(3),"black")) +
  scale_linetype_manual(values = c("solid","solid","solid","dashed")) +
  scale_shape_manual(values = c(15,16,17,18))+
  theme_eddy(23) +
  theme(strip.text.x = element_text(debug=FALSE,size = rel(0.8),margin = margin(3,0,3,0, "pt")),
        legend.key.width = unit(3, "line"))  +
  labs(y="",x="Year",colour="",shape="",linetype="") +
  facet_wrap(~ftitle) +
  guides(colour=guide_legend(ncol = 1))

ggsave(here("plots","3_supply","appendix_migration.pdf"),p_internal_mig + p_international_mig,width=16,height=9)

#====================================================================
#4. Exhibits for the institutions section
#====================================================================

# 4.1 Union Coverage
#--------------------------

#Refactored to make this graph directly from the LIAB log.
fct_tarif <- tribble(
  ~faclevel, ~label,
  1,"Industry union agreement",
  2, "Firm union agreement",
  3, "No union agreement",
  9999, "Missing union information")

p_union_coverage <- merge_mat("tarif") %>%
  parse_tbl() %>%
  group_by(east,year,tarif) %>%
  slice(1) %>%
  filter(as.numeric(tarif)!=9999,
         as.numeric(tarif)!=3) %>%
  filter(year<=2010) %>%
  convert_factors() %>%
  ggplot(aes(x=year,y=rif_share,colour=east,shape=east))+
  facet_wrap(~tarif) +
  geom_line(linewidth=1.4) +
  geom_point(size=2.6) +
  scale_x_continuous(limits=c(1995,2010),n.breaks = 6) +
  scale_y_continuous(label = label_percent(),n.breaks = 6)+
  scale_color_brewer(palette="Set1") +
  theme_eddy(23) +
  theme(strip.text.x = element_text(debug=FALSE,size = rel(0.8),margin = margin(3,0,3,0, "pt")),
        legend.key.width = unit(3, "line")) +
  labs(y="Pct. of workers covered by union agreement",x="Year",colour="",shape="")

ggsave(here("plots","4_institutions","union_coverage.pdf"),p_union_coverage,width=16,height=9) 


# 4.2 Union DFL
#--------------------------


log_union_pct_adjust <- read_st_matrices(here("logs","eb04_dustmann_schoenberg_replication.log"))

#Main Plot for 1997 to 2010
p_union_pct_adjust_main <- bind_rows(log_union_pct_adjust$m_educ_w,
                                     log_union_pct_adjust$m_union_w,.id = "union") %>% 
  mutate(across(!union,parse_number),
         east  = if_else(east==1, "East","West"),
         union = factor(union,
                        levels=c("1","2"),
                        labels=c("Adjusted to 1997 demographic composition",
                                 "Adjusted to 1997 demographic and union composition"))) %>%
  select(-starts_with("N_"),-rownum) %>%
  filter(year %in% c("1997","2010")) %>%
  pivot_wider(id_cols = c("east","pct","union"),names_from = "year",values_from = "value",names_prefix = "w") %>%
  mutate(wg = (w2010 - w1997)/w1997) %>%  
  filter(pct>1 & pct<=85) %>%
  ggplot(aes(x=pct,y=wg,linetype=union,shape=union,colour=union)) +
  geom_line(linewidth=1.4) +
  geom_point(size=2.6) +
  facet_wrap(~east) +
  scale_x_continuous(breaks=seq(5,85,10)) +
  scale_y_continuous(n.breaks = 6, labels=scales::label_percent()) +
  scale_color_brewer(palette="Set1",direction = -1) +
  theme_eddy(23) +
  labs(x="Percentile",y="Wage Growth at percentile \n bewteen 1997 and 2010",linetype="",shape="",color="") + 
  theme(legend.key.width = unit(3, "line")) +
  guides(colour=guide_legend(ncol=1))

ggsave(here("plots","4_institutions","p_union_pct_adjust.pdf"),p_union_pct_adjust_main,width=16,height=9) 

#Main Plot for 1997 to 2004
p_union_pct_adjust_main2004 <- bind_rows(log_union_pct_adjust$m_educ_w,
                                         log_union_pct_adjust$m_union_w,.id = "union") %>% 
  mutate(across(!union,parse_number),
         east  = if_else(east==1, "East","West"),
         union = factor(union,
                        levels=c("1","2"),
                        labels=c("Adjusted to 1997 demographic composition",
                                 "Adjusted to 1997 demographic and union composition"))) %>%
  select(-starts_with("N_"),-rownum) %>%
  filter(year %in% c("1997","2004")) %>%
  pivot_wider(id_cols = c("east","pct","union"),names_from = "year",values_from = "value",names_prefix = "w") %>%
  mutate(wg = (w2004 - w1997)/w1997) %>%  
  filter(pct>1 & pct<=85) %>%
  ggplot(aes(x=pct,y=wg,linetype=union,shape=union,colour=union)) +
  geom_line(linewidth=1.4) +
  geom_point(size=2.6) +
  facet_wrap(~east) +
  scale_x_continuous(breaks=seq(5,85,10)) +
  scale_y_continuous(n.breaks = 6, labels=scales::label_percent()) +
  scale_color_brewer(palette="Set1",direction = -1) +
  theme_eddy(23) +
  labs(x="Percentile",y="Wage Growth at percentile \n bewteen 1997 and 2004",linetype="",shape="",color="") + 
  theme(legend.key.width = unit(3, "line")) +
  guides(colour=guide_legend(ncol=1))


p_union_overall <- bind_rows(log_union_pct_adjust$m_educ_w,
                             log_union_pct_adjust$m_union_w,.id = "union") %>% 
  mutate(across(!union,parse_number),
         east  = if_else(east==1, "East","West"),
         union = factor(union,
                        levels=c("1","2"),
                        labels=c("Adjusted to 1997 demographic composition",
                                 "Adjusted to 1997 demographic and union composition"))) %>%
  select(-starts_with("N_"),-rownum) %>%
  pivot_wider(id_cols = c("east","pct","union"),names_from = "year",values_from = "value",names_prefix = "w") %>%
  mutate(wg_2004 = (w2004 - w1997)/w1997,
         wg_2010 = (w2010 - w2004)/w2004) %>%  
  select(east,pct,union,starts_with("wg_")) %>%
  pivot_longer(cols=starts_with("wg"),names_to="time",values_to = "wg") %>%
  mutate(time = factor(time, 
                       levels = c("wg_2004","wg_2010"),
                       labels = c("Wage growth from 1997 to 2004",
                                  "Wage growth from 2004 to 2010"))) %>%
  filter(pct>1 & pct<=85) %>%
  ggplot(aes(x=pct,y=wg,linetype=union,shape=union,colour=union)) +
  geom_line(linewidth=1.4) +
  geom_point(size=2.6) +
  facet_wrap(~time + east) +
  scale_x_continuous(breaks=seq(5,85,10)) +
  scale_y_continuous(n.breaks = 6, labels=scales::label_percent()) +
  scale_color_brewer(palette="Set1",direction = -1) +
  theme_eddy(23) +
  labs(x="Percentile",y="Wage Growth at percentile",linetype="",shape="",color="") + 
  theme(legend.key.width = unit(3, "line")) +
  guides(colour=guide_legend(ncol=1))

ggsave(here("plots","4_institutions","p_union_pct_adjust_appx.pdf"),p_union_overall,width=16,height=12) 


#Preprocess the tarif development table:
uc_pct <- read_st_matrices(here("logs","eb05_liab_union_coverage_at_diff_pct.log"))  

list(uc_pct$m_branche_whrf,
     uc_pct$m_haus_whrf) %>%
  map(~select(.x,-starts_with("N_"),-rownum)) %>%
  reduce(left_join, by =c("east","year","pct_class")) %>%
  filter(pct_class!=4) %>%
  mutate(year=parse_number(year),
         east=if_else(east==1, "East","West"),
         pct_class = factor(parse_integer(pct_class),
                            levels=1L:3L,
                            labels= c("Below 15","15 to 50","50 to 85")
         ),
         m_none = 1- parse_number(m_branche) -parse_number(m_haus),
         pct_no_tarif = scales::label_percent()(round(m_none,digits = 3)))  %>%
  select(east,year,pct_class,pct_no_tarif) %>% 
  pivot_wider(id_cols = c("year","east"),names_from = "pct_class",values_from = "pct_no_tarif") %>%
  writexl::write_xlsx(here("tables","union_pct_classes_new.xlsx"))

# 4.3 Sectoral minimum wage bite
#---------------------------------

p_mw_bite <- read_dta(here("smalldata","5d_minwage_below_next.dta")) %>%
  bind_rows(read_dta(here("smalldata","5d_minwage_below_next_pct_class.dta")) %>%
              filter(pct_class==0) %>%
              select(-pct_class),.id = "by_pct") %>%
  mutate(east   = if_else(east==1, "East","West"),
         by_pct = factor(by_pct, labels=c("All full-time workers","Workers with wages up to the 15th percentile"))) %>%
  ggplot(aes(x=year,y=fraction,colour=east,shape=east))+
  facet_wrap(~by_pct,scales = "free_y") +
  geom_line(linewidth=1.4) +
  geom_point(size=2.6) +
  scale_x_continuous(limits=c(2008,2013),n.breaks = 5) +
  scale_y_continuous(label = label_percent(),n.breaks = 6)+
  scale_color_brewer(palette="Set1") +
  theme_eddy(23) +
  theme(strip.text.x = element_text(debug=FALSE,size = rel(0.8),margin = margin(3,0,3,0, "pt")),
        legend.key.width = unit(3, "line")) +
  labs(y="Fraction of workers affected by\nmininimum wage next year",x="Year",colour="",shape="")


ggsave(here("plots","4_institutions","mwbite.pdf"),p_mw_bite,width=16,height=9) 

# 4.4 Minimum wage and non-minwage industry densities
#----------------------------------------------------

p_mw_densities <- read_dta(here("smalldata","5e_densities.dta")) %>%
  select(x,starts_with("fx_")) %>%
  pivot_longer(cols = starts_with("fx_"),names_to = "vname",values_to="value") %>%
  mutate(minwage = str_detect(vname,"mw0") %>% factor(labels=c("Minimum wage industries","Non-minimum wage idustries")),
         year    = if_else(str_detect(vname,"2014"),2014L,2008L) %>% factor()) %>%
  filter(x<=3500) %>%
  ggplot(aes(x=x,y=value,colour=minwage,linetype=year))+
  geom_line(linewidth=1.4) +
  facet_wrap(~minwage) +
  scale_color_brewer(palette="Set1") +
  scale_linetype_manual(values =c("dashed","solid"))+
  scale_x_continuous(n.breaks = 8, labels= label_dollar(prefix = "",suffix = "€")) +
  theme_eddy(23) +
  theme(strip.text.x = element_text(debug=FALSE,size = rel(0.8),margin = margin(3,0,3,0, "pt")),
        legend.key.width = unit(3, "line"))+
  guides(colour ="none") +
  labs(x="Monthly real wage",y="Density",linetype="")



ggsave(here("plots","4_institutions","mw_densities.pdf"),p_mw_densities,width=16,height=9) 

#====================================================================
# 5. Exhibits for the demand side section
#====================================================================

p_wgap_contrib_svbroad <- generate_wgap_contrib("svbroad","Sector") %>%
  mutate( wgap = factor(wgap,levels=c("85-50","50-15"),labels=c("85-50 Wage Gap","50-15 Wage Gap")),
          contrib = contrib*100)  %>%
  plot_rif_contrib(.y="contrib",
                   .gvar="svbroad",
                   .ylab="Within-sector contribution\nto overall changes of inequality",
                   .spal = c(20,15,16,17,18,19),
                   .cpal = c(brewer_pal(palette = "Set1")(4),"black"))


p_wgap_contrib_sector <- generate_wgap_contrib("svbroad","Sector") |>
  mutate(sector = if_else(svbroad %in% c("Public Administration and Education","Services (Private)"),
                          "Services",svbroad)) |>
  group_by(wgap,east,year,sector) |>
  summarise(contrib = sum(contrib)) |>
  mutate( wgap = factor(wgap,levels=c("85-50","50-15"),labels=c("85-50 Wage Gap","50-15 Wage Gap")),
          contrib = contrib*100,
          sector = factor(sector,levels=c("Manufacturing","Construction","Services","Sector Composition effects"))) |>
  plot_rif_contrib(.y="contrib",
                   .gvar="sector",
                   .ylab="Within-sector contribution\nto overall changes of inequality",
                   .spal = c(15,16,17,18),
                   .cpal = c(brewer_pal(palette = "Set1")(3),"black"),
                   .lpal = c(rep("solid",3),"dashed"))

ggsave(here("plots","5_demand","rif_contrib_wgap_sector.pdf"),p_wgap_contrib_sector,width=16,height=16)
ggsave(here("plots","5_demand","rif_contrib_wgap_svbroad.pdf"),p_wgap_contrib_svbroad,width=16,height=16)

         
#Sector share figure for the appendix
p_sector_shares <- rif_pct[["svbroad"]] |>
  filter(pct==50) |>
  mutate(sector = if_else(svbroad %in% c("Public Administration and Education","Services (Private)"),
                          "Services",svbroad)) |>
  group_by(east,year,sector) |>
  summarise(share = sum(rif_share), .groups="drop") |>
  mutate(sector = factor(sector,levels=c("Manufacturing","Construction","Services")))|>
  ggplot(aes(x=year, y=share, colour=sector, shape=sector)) +
  geom_line(linewidth=1.4) +
  geom_point(size=2.6) +
  facet_wrap(~east) +
  theme_eddy(23) +
  theme(text=element_text(size=22)) +
  scale_color_brewer(palette = "Set1")+
  scale_shape_manual(values = c(15,16,17)) +
  scale_y_continuous(labels=label_percent(), n.breaks=12) +
  labs(y="Employment share of sector",x="Year",colour="",shape="") +
  guides(colour = guide_legend(ncol = 1))

ggsave(here("plots","5_demand","sector_shares.pdf"),p_sector_shares,width=16,height=9)

#5b. DFL by occupations
#----------------------------------

#DFL by occupation
p_dfl_occ3 <- pct_dfl |>
  filter(var == "occ3") |>
  left_join(pct_act, by=c("east","year","pct")) |>
  select(-n_persnr,-var) |>
  pivot_wider(id_cols =c("east","year"),
              names_from = "pct",
              values_from = c("p","dfl_p")) |>
  mutate(across(starts_with("p")|starts_with("dfl_p"), log),
         wg_act_85_50 = p_85-p_50,
         wg_dfl_85_50 = dfl_p_85 -dfl_p_50,
         wg_act_50_15 = p_50-p_15,
         wg_dfl_50_15 = dfl_p_50 -dfl_p_15) |>
  select(east,year,starts_with("wg")) %>%
  pivot_longer(starts_with("wg"),names_to = "var",values_to = "value") |>
  mutate(wage_gap = if_else(str_detect(var,"85_50"),"85-50 Wage Gap","50-15 Wage Gap"),
         dfl      = if_else(str_detect(var,"dfl"),"Adjusted to 1995 Occupation Structure","Actual wage distribution")) |>
  ggplot(aes(y=value,x=year,colour=wage_gap,shape=wage_gap,linetype=dfl)) +
  geom_line(linewidth=1.4) +
  geom_point(size=2.6) +
  facet_wrap(~east) +
  theme_eddy(23) +
  theme(text=element_text(size=22)) +
  #scale_colour_manual(values = c(brewer_pal(palette = "Set1")(2),"#282828"))+
  scale_color_brewer(palette = "Set1")+
  scale_linetype_manual(values=c("solid","dashed")) +
  scale_y_continuous(n.breaks = 8) +
  labs(x="Year",           
       y ="Inequality measure",
       colour ="",
       shape  ="",
       linetype="") +
  guides(colour   = guide_legend(ncol = 1),
         linetype = guide_legend(ncol = 1)) + 
  theme(legend.key.width = unit(3, "line"))

ggsave(here("plots","5_demand","dfl_occ3.pdf"),p_dfl_occ3,width=16,height=9)


