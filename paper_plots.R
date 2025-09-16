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

source(here("general_functions.R"))
source(here("factor_definitions.R"))


#Read Info from stata outputs
log_main  <- read_st_matrices(here("logs","eb01_overview.log"))


#====================================================================
#   2. Exhibits for the stylized facts section
#====================================================================

p_wg_trend <- log_main$m_pct |>
  mutate(across(everything(),parse_number),
         east = if_else(east==1, "East","West")) |>
  mutate(wg85_50 = log(p85/p50),
         wg50_15 = log(p50/p15)) |>
  left_join(log_main$m_sd_educ  |>
              mutate(across(everything(),parse_number),
                     east = if_else(east==1, "East","West")) |>
              select(east,year,sd)
            ,by=c("year","east")) |>
  select(east,year,wg85_50,wg50_15,sd) |>
  pivot_longer(cols     = c("wg85_50","wg50_15","sd"),
               names_to = "ineq",
               values_to = "values") |>
  mutate(ineq = case_when(ineq == "sd" ~ "Std. Deviation of Wages",
                          ineq == "wg85_50" ~ "85-50 Wage Gap",
                          ineq == "wg50_15" ~ "50-15 Wage Gap"
  )) |>
  ggplot()+
  aes(x=year,y=values,colour=ineq,shape=ineq, linetype=ineq) +
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




ggsave(here("plots","2_stylized_facts","indexed_wage_growth.pdf"),indexed_pct_plots[["m_pct"]],width=16,height=9)

log_main$m_pct |>
  mutate(across(everything(),parse_number),
         east = if_else(east==1, "East","West")) |>
  pivot_longer(cols         = starts_with("p"),
               names_to     = "pct",
               names_prefix = "p",
               values_to    = "p") |>
  mutate(pct  = parse_integer(pct)) |>
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



