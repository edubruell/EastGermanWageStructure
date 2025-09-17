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
log_old <- read_st_matrices(here("logs","eb_03_results_export_R.log")) 

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

pct_act <- log_main$m_pct |>
  mutate(across(everything(),parse_number),
         east = if_else(east==1, "East","West")) |>
  pivot_longer(cols         = starts_with("p"),
               names_to     = "pct",
               names_prefix = "p",
               values_to    = "p") |>
  mutate(pct  = parse_integer(pct))

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

comp_actual_wgap |> left_join(
  log_old$m_wgtrend |>
    select(-rownum, -sd) |>
    mutate(across(everything(), parse_number),
           east = if_else(east==1, "East","West")) |>
    pivot_longer(starts_with("wg"),names_prefix = "wg", 
                 names_to="wgap",
                 values_to ="old_w"
                 ) |>
    mutate(wgap = str_replace(wgap,"_","-")), by=c("east","year","wgap")) |>
  filter(var=="age_educ") |>
  select(east,year,wgap,new=w,old=old_w) |>
  pivot_longer(cols = c("new","old")) |>
  ggplot()+
  aes(x=year,y=value,colour=wgap,shape=wgap, linetype=name) +
  geom_line(linewidth=1.4) +
  geom_point(size=2.6) +
  facet_wrap(~east) +
  theme_eddy(23) +
  theme(text=element_text(size=22)) +
  scale_color_brewer(palette = "Set1")+
  scale_linetype_manual(values=c("solid","dashed")) +
  scale_y_continuous(n.breaks = 8)+
  labs(y= "Measures of Dispersion",
       x= "Year",
       colour ="Inequality measure",
       shape  ="Inequality measure",
       linetype = "Old/New Data Prepare") +
  guides(colour = guide_legend(title.position = "top",title.hjust=0.5,ncol=3),
         linetype = guide_legend(title.position = "top",title.hjust=0.5,ncol=3)
         )


indexed_old <- log_old$m_pct |>
  mutate(across(everything(),parse_number),
         east = if_else(east==1, "East","West")) |>
  pivot_longer(cols         = starts_with("p"),
               names_to     = "pct",
               names_prefix = "p",
               values_to    = "p") |>
  mutate(pct  = parse_integer(pct)) |>
  select(-starts_with("n")) |>
  select(pct,east,year,pct, p) |>
  group_by(east,pct) |>
  arrange(east,pct,year) |>
  mutate(i_p = exp(p)/first(exp(p)),
         pct  = glue("{pct}th percentile")) |>
  ungroup() |>
  transmute(new = "Old",east,year,pct,i_p)

pct_act |>
  group_by(east,pct) |>
  arrange(east,pct,year) |>
  mutate(i_p = p/first(p),
         pct  = glue("{pct}th percentile"))  |>
  ungroup() |>
  transmute(new = "New",east,year,pct,i_p) |>
  bind_rows(indexed_old) |>
  ggplot() +
  aes(x=year,y=i_p,colour=pct,shape=pct,linetype = new) +
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
       shape ="Percentile",
       linetype="Old/New Data Prepare") +
  guides(colour = guide_legend(title.position = "top",title.hjust=0.5,ncol=3),
         linetype = guide_legend(title.position = "top",title.hjust=0.5,ncol=3))

