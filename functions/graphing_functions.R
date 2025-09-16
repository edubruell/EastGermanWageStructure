

#---------------------------------------------------------------------------------
# 1. Analytic Plots
#---------------------------------------------------------------------------------

calc_error_graph<-function(varname,in_p=85){
   p <-rif_pct[[varname]] %>%
    filter(pct==in_p) %>%
    mutate(p_rmain = rif_value * rif_share,
           p_rdfl  = ffl_value * ffl_share) %>%
    group_by(east,year) %>%
    summarise(across(starts_with("p_") | rif_share, sum), .groups="drop") %>%
    left_join(pct_dfl %>% filter(pct==in_p & var==varname), by=c("east","year")) %>%
    select(-pct, -var) %>%
    left_join(pct_nm %>% filter(pct==in_p) %>% select(-pct), by=c("east","year")) %>%
    rename(p_act = p_nm, p_dfl = dfl_p) %>%
    mutate(diff_act_dfl = p_act - p_dfl,
           diff_r       = p_rmain - p_rdfl) %>%
    print() %>%
    ggplot(aes(x=year)) + 
    geom_line(aes(y=p_rmain,linetype="Computed from RIF" ,colour="RAW"), size=1.2) +
    geom_line(aes(y=p_act,  linetype="Actual"            ,colour="RAW"), size=1.2) + 
    geom_line(aes(y=p_dfl,  linetype="Computed from RIF" ,colour="DFL"), size=1.2) +
    geom_line(aes(y=p_rdfl, linetype="Actual"            ,colour="DFL"), size=1.2) +
    ylab(glue("{in_p}. percentile")) +
    xlab("year") +
    facet_grid(~east) +
    theme_eddy() + 
    guides(colour  = guide_legend(title="Adjustment", title.position = "top"),
           linetype = guide_legend(title="Computation", title.position = "top")) 
  
  
    p %>% ggsave(plot = .,here("auto_plots","A1_calc_error",glue("{in_p}_{varname}.pdf")), width = 16 , height=9)
    p %>% print()
}  


draw_group_shares <- function(varname,vtitle){
  print(glue("Drawing group share graph for {vtitle}"))
  p <- rif_pct[[varname]] %>%
    filter(pct==50) %>%
    ggplot(aes(x=year, colour=.data[[varname]])) +
    geom_line(aes(y=rif_share,  linetype="Actual"), size=1.2) + 
    geom_line(aes(y=ffl_share,  linetype="DFL"), size=1.2) +
    ylab(glue("{vtitle} shares over time")) +
    xlab("year") +
    facet_grid(~east) +
    theme_eddy()+
    scale_y_continuous(label = percent_format())+
    guides(colour   = guide_legend(title=vtitle, title.position = "top"),
           linetype = guide_legend(title="Weighting", title.position = "top"))  
  
    p %>% ggsave(plot = .,here("auto_plots","A2_group_shares",glue("{varname}.pdf")), width = 16 , height=9)
    p %>% print()
} 

#Combined Error Analysis Plot
draw_error_analytics<-function(vname,in_wg){
  p<- rif_wgap[[vname]] %>% 
    ffl_contrib_new(vname,1995,"wgap")  %>%
    filter(wgap ==in_wg) %>%
    print() %>%
    ggplot(aes(x=year)) +
    #geom_line(aes(y=wgap_test            ,     colour = "Self-computed Test (Function)"),linetype="dotted", size=1.2)+
    geom_line(aes(y=w_dfl-c_w_dfl        ,     colour = "DFL W-gap increase (Actual)"), size=1.2)+
    geom_line(aes(y=ws_total+w-w_dfl     ,     colour = "DFL comp. + Total WS Effects"), linetype="dashed", size=1.2)+
    geom_line(aes(y=comp_total           ,     colour = "Composition Effect within FFL \n (That should be close to zero)"), linetype="dashed", size=1.2)+
    geom_line(aes(y=ws_total             ,     colour = "Total Wage Structure Effects"),  size=1.2)+
    geom_line(aes(y=w- c_w      ,     colour = "Increase actual"), linetype="dotdash", size=1.2)+
    geom_line(aes(y=w- w_dfl      ,     colour = "DFL composition effect"), linetype="dotdash", size=1.2)+
    guides(color= guide_legend(title=glue("Test of DFL procedure"), title.position = "top", nrow = 2)) +
    theme_eddy() +
    ylab(glue("Increases in {in_wg} Wage-gap"))+
    xlab("Year") +
    facet_grid(~east) 
  
  p %>% ggsave(plot = .,here("auto_plots","A4_combined_wgap_analytics",glue("{in_wg}_{vname}.pdf")), width = 16 , height=9)
  p %>% print()
}


#draw_error_analytics("fctrl","85-50")

#---------------------------------------------------------------------------------
# 2. Draw a DFL Graph
#---------------------------------------------------------------------------------

draw_dfl_graph <- function(varname,vartitle){
  
  #create an internal dataset restructured for the graph
  int_tbl <- comp_actual_pct %>%
    filter(var==varname) %>%
    pivot_longer(cols=ends_with("p"), names_to = "dfl", values_to="p") %>%
    mutate(dfl = if_else(dfl=="dfl_p",glue("DFL-Adjusted for {vartitle}--composition"),"Actual")) %>%
    group_by(east,pct,dfl) %>%
    arrange(east,pct,dfl,year) %>%
    mutate(i_p = p/p[1]) %>%
    ungroup()
  
  #Draw DFL indexed percentiles 
  glue("Draw indexed percentile DFL-Graph for {vartitle}") %>% print()
  p_pct <- int_tbl%>%
    ggplot(aes(x=year,y=i_p, colour=glue("{pct}. percentile"), linetype=dfl)) +
    geom_line(size = 1.2) +
    facet_grid(~east) +
    theme_eddy() + 
    ylab("Indexed percentile") +
    xlab("Year") +
    theme(legend.position = "bottom",
          strip.text.x = element_text(size = 18),
          #strip.background = element_rect(color="black", fill="grey", linetype="solid")
    ) +
    scale_y_continuous(label = percent_format()) +
    #scale_colour_zew(palette = "mixed")+
    guides(colour = guide_legend(title="Percentile", title.position = "top"),
           linetype = guide_legend(title="Adjustment", title.position = "top")) 
    

  p_pct %>% ggsave(plot = .,here("plots","4_dfl_comp",glue("dfl_{varname}.pdf")), width = 16 , height=9)
  
  #Draw DFL wage gap graph 
  glue("Draw wage gap DFL-Graph for {vartitle}") %>% print()
  
  p_wgap <- int_tbl%>%
    select(-i_p) %>%
    ungroup() %>%
    pivot_wider(id_cols = c("east","dfl","year"),
                names_from="pct",
                names_prefix="p",
                values_from="p") %>%
    mutate(wgap_85_50 = log(p85/p50),
           wgap_50_15 = log(p50/p15)) %>%
    ggplot(aes(x=year,linetype=dfl)) +
    geom_line(aes(y=wgap_85_50, colour="85-50 Log-Wage Gap"), size=1.2) +
    geom_line(aes(y=wgap_50_15, colour="50-15 Log-Wage Gap"), size=1.2) +
    facet_grid(~east)+
    theme_minimal(base_size = 16) + 
    #scale_colour_zew()+
    ylab("Log Wage Gaps") +
    xlab("Year") +
    theme_eddy() +
    #scale_y_continuous(label = percent_format()) +
    guides(colour   = guide_legend(title="Inequality Measure", title.position = "top"),
           linetype = guide_legend(title="Adjustment", title.position = "top")) 

  
  p_wgap %>% ggsave(plot = .,here("plots","4_dfl_comp",glue("wgap_dfl_{varname}.pdf")), width = 16 , height=9)
}

draw_dfl_indexed <- function(vname,vtitle){
  p_iwgap <-  comp_actual_pct %>%
    filter(var==vname) %>%
    pivot_longer(cols=ends_with("p"), names_to = "dfl", values_to="p") %>%
    mutate(dfl = if_else(dfl=="dfl_p",glue("DFL-Adjusted for {vtitle}--composition"),"Actual")) %>%
    pivot_wider(id_cols = c("east","dfl","year"),
                names_from="pct",
                names_prefix="p",
                values_from="p") %>%
    mutate(wgap_85_50 = log(p85/p50),
           wgap_50_15 = log(p50/p15)) %>%
    get_ref_year_values(1995,c("east","dfl")) %>%
    mutate(i_85_50 = wgap_85_50 - c_wgap_85_50,
           i_50_15 = wgap_50_15 - c_wgap_50_15) %>%
    ggplot(aes(x=year,linetype=dfl)) +
    geom_line(aes(y=i_85_50, colour="85-50 Log-Wage Gap"), size=1.2) +
    geom_line(aes(y=i_50_15, colour="50-15 Log-Wage Gap"), size=1.2) +
    facet_grid(~east)+
    theme_minimal(base_size = 16) + 
    #scale_colour_zew()+
    ylab("Indexed Log Wage Gaps") +
    xlab("Year") +
    theme_eddy() +
    #scale_y_continuous(label = percent_format()) +
    guides(colour   = guide_legend(title="Inequality Measure", title.position = "top"),
           linetype = guide_legend(title="Adjustment", title.position = "top")) 
  
    p_iwgap %>% ggsave(plot = .,here("plots","4_dfl_comp",glue("i_wgap_dfl_{vname}.pdf")), width = 16 , height=9)
}

#draw_dfl_indexed("educ","education")
#draw_dfl_indexed("tw_fe","Two-Way AKM FE Interactions")

#---------------------------------------------------------------------------------
# 2. FFL Plots
#---------------------------------------------------------------------------------

#Function to draw the ffl WS contribution to indexed percentiles
draw_ffl_pct <- function(vname,vtitle,in_pct,cyear){
  print(glue("Draw rif-contributions for {vtitle} at the {in_pct} percentile"))
  p<- rif_pct[[vname]] %>% 
    ffl_contrib(vname,cyear,"pct") %>%
    filter(pct == in_pct) %>%
    arrange(.data[[vname]]) %>%
    ggplot(aes(x=year,colour=.data[[vname]])) +
    geom_line(aes(y=i_p - i_ws_total - 1, linetype= "Overall Composition"),colour="gray", size=1.2) +
    geom_line(aes(y=i_ws , linetype= "Wage structure"), size=1.2) +
    facet_grid(~east) +
    theme_eddy() +
    ylab(glue("Contribution to indexed {in_pct}-percentile change")) +
    xlab("Year") + 
    scale_linetype_manual(values=c("twodash","solid")) +
    scale_y_continuous(label = percent_format()) +
    scale_colour_brewer(palette = "Dark2") + 
    guides(colour   = guide_legend(title=vtitle, title.position = "top"),
           linetype = guide_legend(title="Effect type", title.position = "top")) 

   p %>% ggsave(plot= .,here("plots","5a_rif_pct_contributions",glue("ws_contrib_{in_pct}_{vname}.pdf")), width = 16 , height=9)
   p %>% print()
}

#draw_ffl_pct("educ","Education",50,1995)

#Function to draw the ffl WS contribution to the log wage gaps
draw_ffl_wgap <- function(vname,vtitle,in_wg, cyear, verbose =FALSE){
  wg_fname <- gsub("-","_",in_wg)
  print(glue("Draw ffl-contributions for {vtitle} at the {in_wg}-log-wage gap"))
  
  #Get the rif-contribs
  int_tbl <- generate_wgap_ws(vname,vtitle,in_wg,cyear) %>% # Conditional pipe if verbose is TRUE
    { if(verbose == TRUE) print(.,n = Inf)  else . } 
  
  #Get the right colours
  ws_colours <- int_tbl %>% 
    filter(as.numeric(vtype) == 1L) %>%  
    select(.data[[vname]]) %>%
    distinct() %>% 
    pull(.data[[vname]]) %>% 
    length() %>%
    brewer_pal(type = "seq", palette="Dark2")(.)
  
  #make the plot
  p<-int_tbl %>% 
    arrange(.data[[vname]]) %>%
    ggplot(aes(x=year,colour=.data[[vname]], linetype = vtype)) +
    geom_line(aes(y=ws), size=1.2) +
    facet_grid(~east) +
    theme_eddy() +
    ylab(glue("Contribution to log-point increase of {in_wg} log-wage gap")) +
    xlab("Year") + 
    scale_colour_manual(values = c(ws_colours,"#808080"))+
    scale_linetype_manual(values=c("solid","dashed"), guide = FALSE) +
    guides(colour   = guide_legend(title=vtitle, 
                                   title.position = "top",
                                   override.aes = list(linetype = c(rep(1,length(ws_colours)),2)),
                                   nrow = 2, byrow = TRUE)) +
    theme(legend.key.size = grid::unit(2.2, "lines")) 
  
  
  
  p %>% ggsave(plot= .,here("plots","5b_rif_wgap_contributions",glue("ws_contrib_{wg_fname}_{vname}.pdf")), width = 16 , height=9)
  p %>% print()
}

plot_rif_contrib <- function(.tbl,
                       .y,
                       .gvar,
                       .cpal = NULL,
                       .lpal = NULL,
                       .spal = NULL,
                       .ylab,
                       .pct  = FALSE){
  
  .nlevels <- .tbl %>%
    distinct(.data[[.gvar]]) %>%
    pull()%>%
    length()
  
  if(.pct==TRUE){.fname <- "pct"}
  if(.pct==FALSE){.fname <- "wgap"}
  
  if( is.null(.cpal)){.cpal <- c(brewer_pal(palette = "Set1")(.nlevels-1),"black")}
  if( is.null(.lpal)){.lpal <- c(rep("solid",.nlevels-1),"dashed")}
  if(!is.null(.spal)){.spal <- scale_shape_manual(values = .spal)}  
  
  .tbl %>%
    ggplot(aes(y=.data[[.y]],x=year,
               colour=.data[[.gvar]],
               shape=.data[[.gvar]],
               linetype=.data[[.gvar]])) +
    geom_line(linewidth=1.4) +
    geom_point(size=2.6) +
    facet_wrap(~ .data[[.fname]] + east, ncol = 2) +
    theme_eddy(23) +
    theme(strip.text.x = element_text(debug=FALSE,size = rel(0.8),margin = margin(3,0,3,0, "pt"))) +
    scale_color_manual(values = .cpal) +
    {if(.pct==TRUE) scale_y_continuous(labels = label_percent(),n.breaks =  6)}+
    {if(.pct==FALSE)  scale_y_continuous(labels = label_dollar(prefix = "",suffix="p.p."),n.breaks =  6)}+
    scale_linetype_manual(values=.lpal) +
    .spal + 
    labs(x="Year",           
         y =.ylab,
         colour ="",
         shape  ="",
         linetype="") +
    guides(colour = guide_legend(ncol = 1))
  
}

#Alternative plot command for RIF contributions
plot_bars_rif_contrib <- function(.tbl,
                                  .y,
                                  .gvar,
                                  .ylab,
                                  .pct     = FALSE,
                                  .cpal    = NULL,
                                  .cpalcom = brewer_pal(palette="Set1")){
  
  .nlevels <- .tbl %>%
    distinct(.data[[.gvar]]) %>%
    pull()%>%
    length()
  
  if(.pct==TRUE){.fname <- "pct"}
  if(.pct==FALSE){.fname <- "wgap"}
  
  if( is.null(.cpal)){.cpal <- c(.cpalcom(.nlevels-1),"darkgrey")}
  
  .totals <- .tbl %>%
    group_by(.data[[.fname]],east,year) %>%
    summarise(measure = sum(contrib), .groups="drop")
  
  
  .tbl %>%
    ggplot(aes(y=.data[[.y]],
               x=year)) +
    geom_bar(mapping = aes(fill=.data[[.gvar]]),position="stack",stat = "identity") +
    geom_line(data = .totals,aes(y=measure),color="black",linewidth=1.4) +
    geom_point(data = .totals,aes(y=measure),color="black",size=2.6) +
    #geom_hline(yintercept = 0, color="lightgray", size=1.4,linetype="dashed")+
    facet_wrap(~ .data[[.fname]] + east, ncol = 2) +
    theme_eddy(23) +
    theme(strip.text.x = element_text(debug=FALSE,size = rel(0.8),margin = margin(3,0,3,0, "pt"))) +
    scale_fill_manual(values = .cpal) +
    {if(.pct==TRUE)   scale_y_continuous(labels = label_percent(),n.breaks =  6)}+
    {if(.pct==FALSE)  scale_y_continuous(labels = label_dollar(prefix = "",suffix="p.p."),n.breaks =  6)} +
    labs(x="Year",           
         y =.ylab,
         fill ="") +
    guides(fill = guide_legend(ncol = 1))
}


#draw_ffl_wgap("svbroad","Industrial sectors (with broad services distinction)","85-50",1995)
