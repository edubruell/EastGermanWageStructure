
#-----------------------------------------------------
#   1. Define a read function for stata log matrices
#-----------------------------------------------------

#A function to read matrices from a stata log file
read_st_matrices <-function(filename){
  v <- readLines(filename)
  
  #Find lines where a matrix command starts
  matrix_start_lines <- grep(". matrix list",v)
  #Keep only the ones where the next line is empty
  matrix_start_lines <- matrix_start_lines[v[matrix_start_lines+1] == ""] + 2
  
  #Find matrices 
  matrices <- as_tibble(v[matrix_start_lines]) %>%
    separate(value,c("names","m_spec"),sep="\\[")  %>%
    mutate(m_spec = str_remove(m_spec,"\\]"))  %>%
    separate(m_spec,c("n_rows","n_cols"),sep=",") %>%
    mutate(start_lines = matrix_start_lines + 1,
           n_cols      = parse_integer(n_cols),
           n_rows      = parse_integer(n_rows),
           end_lines   = start_lines + n_rows) %>%
    print()
  
  #A function to read the sepearate lines of log_matrices
  read_st_logmatrix <- function(first_line,last_line){
    c_names <-  str_split(v[first_line], "\\s+") %>% unlist()
    c_names[1] <- "rownum"
    tbl <- v[seq(first_line+1,last_line)] %>% 
      as_tibble() %>% 
      mutate(value = str_trim(value,"left")) %>% 
      separate(value,c_names,sep="\\s+")
    return(tbl)
  }
  
  #Read the matrices
  lmtr<-map2(matrices$start_lines,matrices$end_lines,read_st_logmatrix)
  
  #Get the names into a vector
  names(lmtr) <- matrices$names
  return(lmtr)
}

#-----------------------------------------------------
#   2. Parsing functions for wgap and percentile data 
#-----------------------------------------------------


#A function to merge s_ and r_ matrices back to one matrix
merge_mat <- function(name,otf=TRUE){
  r_tbl <- log_r[[name]]
  s_tbl <- log_s[[name]]
  
  #On the fly calculated RIFs have seperate RIF values for ffl and unweighted
  if(otf==FALSE){
    int_tbl <- rbind(r_tbl %>% 
                       mutate(ffl = "0"), 
                     r_tbl %>% 
                       mutate(ffl = "1"))
  }else{
    int_tbl <- r_tbl
  }
  
  
  #Combine the s_ and r_ matrices back to one
 int_tbl%>%
    select(-rownum, -starts_with("n_")) %>%
    left_join(s_tbl, by=c("east","year",name,"ffl")) %>%
    select(-rownum)
}


#A fucntion to parse non numeric values in all tibbles for the percentiles outcomes

parse_tbl <- function(tbl, ptype="pct"){
  if(ptype=="pct"){
    c_pfx  <- "rif_"
    c_name <- "pct"
    last_steps <- function(tbl){
      tbl %>%
      mutate(pct = parse_integer(pct))
    }
  }else{
    c_pfx <- "w_"
    c_name <- "wgap"
    last_steps <- function(tbl){
      tbl %>%
        mutate(wgap = gsub("_","-",wgap))
    }
  }
  
  tbl %>% mutate(year = parse_number(year),
                 ffl  = if_else(ffl=="1","ffl","rif"),
                 across(starts_with(c_pfx) | share | east | starts_with("n"), parse_number),
                 east = if_else(east==1, "East","West")) %>%
    pivot_longer(cols=starts_with(c_pfx),names_to = c_name, names_prefix = c_pfx, values_to = "value") %>%
    relocate(all_of(c_name), .before= east)  %>%
    pivot_wider(id_cols = 1:4, 
               names_from="ffl", 
                values_from=c("value","share"),
                names_glue = "{ffl}_{.value}") %>%
    last_steps()
}



#---------------------------------------------------------------------
#   3. A simple function to define factors for the missing variable
#--------------------------------------------------------------------

convert_factors<-function(tbl,raw_name = NULL){
  if(is.null(raw_name)){raw_name <- setdiff(colnames(tbl),c("east","year","rif_value","rif_share","ffl_share","ffl_value","pct","wgap"))}
  fct_name <- glue("fct_{raw_name}")
  
  #Get factor labels and levels
  c_levels <- get(fct_name) %>% pull(faclevel)
  c_labels <- get(fct_name) %>% pull(label)
  
  #The line to return 
  tbl %>% 
    mutate({{raw_name}} := parse_integer(.data[[raw_name]]),
           {{raw_name}} := factor(.data[[raw_name]], c_levels, c_labels)) 
}


#A function to collapse factors to simpler subfactors
add_collapsed_fct <- function(.tbl_list,.vname,.cfct){
  #Get a tibble of factor collapse definitions
  collapse_definition <- get(glue("fcol_{.vname}_{.cfct}")) 
  nfac_tbl <- collapse_definition %>% 
    select(ocode, {{ .cfct }} := nlevel) 
  
  #Get the new factor labels and names
  c_levels <- collapse_definition %>% select(nlevel,nfacname) %>% distinct() %>% pull(nlevel)
  c_labels <- collapse_definition %>% select(nlevel,nfacname) %>% distinct() %>% pull(nfacname)
  
  #What kind of matrix a we using here (needed for arrange statements)
  ctype <- colnames(.tbl_list[[.vname]])[1] 
  
  #Get a base tibble from a list to collapse
  tbl <- .tbl_list[[.vname]] %>%
    mutate(ocode = as.numeric(.data[[.vname]])) %>%
    select(-all_of(.vname)) %>%
    left_join(nfac_tbl, by ="ocode") %>%
    select(-ocode) %>%
    mutate({{ .cfct }} := factor(.data[[.cfct]], c_levels, c_labels)) %>%
    group_by(.data[[ctype]],east,year,.data[[.cfct]]) %>%
    arrange(.data[[ctype]],east,year,.data[[.cfct]]) %>%
    mutate(g_rif_share  = rif_share/sum(rif_share),
           g_ffl_share  = ffl_share/sum(ffl_share),
           test         = sum(g_rif_share),
           rif_weighted = rif_value * g_rif_share,
           ffl_weighted = ffl_value * g_ffl_share) %>%
    summarise(across(ends_with("share")|ends_with("weighted"),sum),.groups = "drop") %>%
    select(-starts_with("g")) %>%
    rename(rif_value = rif_weighted,
           ffl_value = ffl_weighted) %>%
    relocate(all_of(ctype),east,year,all_of(.cfct),rif_value,ffl_value,rif_share,ffl_share)
  
  #Add new tibble with collapsed factor to a tibble and combine lists
  .ntbl <- list(tbl)
  names(.ntbl) <- .cfct
  c(.tbl_list,.ntbl)
}


#---------------------------------------------------------------------------------
# 4. Functions to add missing rows to comp_actual where I did not export the DFL decomp
#---------------------------------------------------------------------------------

add_missing_names <- function(tbl,input_names){
  flag <- FALSE
  if(length(input_names) > 1){
    for(name in input_names){
      tbl<- bind_rows(tbl,
              tbl %>% 
                filter(var=="industry") %>%
                mutate(across(ends_with("dfl")|starts_with("dfl"),~as.numeric(NA)),
                       var=name))
    }
    flag <- TRUE
  }else{
   tbl <-  bind_rows(tbl,
              tbl %>% 
                filter(var=="industry") %>%
                mutate(across(ends_with("dfl")|starts_with("dfl"),~as.numeric(NA)),
                       var=input_names))
   flag <- TRUE
  }
  
  tbl %>%
    arrange()
  
  if(flag){return(tbl)}
} 


 
#---------------------------------------------------------------------
#     5. FFL Contribution functions
#--------------------------------------------------------------------

#Simple helper function to get base_year values for all variables but 
#the id_cols into a tibble
get_ref_year_values <-function(tbl,refyear,idcols){
  tbl %>% 
    left_join(tbl %>%
                filter(year==refyear) %>%
                select(-year) %>%
                rename_with(~str_c("c_",.,""), !all_of(idcols)), by=idcols)
}


ffl_contrib <- function(tbl,vname,cyear,ptype="pct"){
  
  #Last steps depend on "wgap" or "pct"
  if(ptype == "pct"){
    last_steps_pt <- function(tbl){
      tbl %>% mutate(
        i_ws    = ws/c_p,
        i_comp  = comp/c_p,
        i_p     = p/c_p,
        i_p_dfl = dfl_p/c_p) %>% 
        group_by(east,pct,year) %>%
        mutate(i_ws_total   = sum(i_ws,na.rm=TRUE),   
               i_comp_total = sum(i_comp,na.rm=TRUE), 
               i_p_test     = sum(i_ws + i_comp,na.rm=TRUE)) %>%
        ungroup() 
    }
  } else {
    last_steps_pt <- function(tbl){
      tbl %>%
        group_by(east,wgap,year) %>%
        mutate(ws_total    = sum(ws,na.rm=TRUE),   
               comp_total  = sum(comp,na.rm=TRUE), 
               wgap_test   = sum(ws + comp,na.rm=TRUE)) %>%
        ungroup()
    }
  }
  #Output the FFL contriubtion to percentiles or wage gaps
  tbl %>%
    left_join(get(glue("comp_actual_{ptype}"))%>% 
                filter(var==vname) %>%
                select(-var), by=c("east","year",ptype)) %>%
    get_ref_year_values(cyear,c(ptype,"east",vname)) %>% 
    mutate(ws   = (ffl_value)  * ffl_share - (c_ffl_value * c_ffl_share),
           comp  = (ffl_value) * (rif_share-ffl_share)) %>%
    last_steps_pt()
}



ffl_contrib_new <- function(tbl,vname,cyear,ptype="pct"){
  
  #Last steps depend on "wgap" or "pct"
  if(ptype == "pct"){
    last_steps_pt <- function(tbl){
      tbl %>% mutate(
        i_ws    = ws/c_p,
        i_comp  = comp/c_p,
        i_p     = p/c_p,
        i_p_dfl = dfl_p/c_p) %>% 
        group_by(east,pct,year) %>%
        mutate(i_ws_total   = sum(i_ws,na.rm=TRUE),   
               i_comp_total = sum(i_comp,na.rm=TRUE), 
               i_p_test     = sum(i_ws + i_comp,na.rm=TRUE)) %>%
        ungroup() 
    }
  } else {
    last_steps_pt <- function(tbl){
      tbl %>%
        group_by(east,wgap,year) %>%
        mutate(ws_total    = sum(ws,na.rm=TRUE),   
               comp_total  = sum(comp,na.rm=TRUE), 
               wgap_test   = sum(ws + comp,na.rm=TRUE)) %>%
        ungroup()
    }
  }
  #Output the FFL contriubtion to percentiles or wage gaps
  tbl %>%
    left_join(get(glue("comp_actual_{ptype}"))%>% 
                filter(var==vname) %>%
                select(-var), by=c("east","year",ptype)) %>%
    get_ref_year_values(cyear,c(ptype,"east",vname)) %>% 
    mutate(ws   = (ffl_value)  * ffl_share - (c_ffl_value * c_ffl_share),
           comp  = (ffl_value) * (c_ffl_share-ffl_share)) %>%
    last_steps_pt()
}






#-------------------------------------------------------------------------------------
#     5. FFL Contribution WGAP function to include overall composition and spec error
#-------------------------------------------------------------------------------------

#Small helper function to copy table rows and replace ws with the first value of a source column
copy_tab_row <- function(tbl,.vname,.source,.f_level,.f_label,.vt){
  tbl %>%
    add_row(slice(.,1) %>% 
              mutate(ws   = .[[.source]][1],
                     {{ .vname }} := factor(.f_level,levels=.f_level, labels=.f_label),
                      vtype = .vt)
    )
}



#A legacy Function to generate tidy effect lists for the output of FFL contributions
generate_wgap_ws <- function(vname,vtitle,in_wg,byear=1995){

  
  #Warning for later refactoring comp_total refers to the composition effect on w_dfl
rif_wgap[[vname]] %>% 
  #Occ2 needs this since 2011 has all sorts of problems with the occupation coding!
  { if(vname=="occ2"|vname=="occ1") filter(.,year!=2011)  else . } %>%
  { if(vname=="occ2") filter(.,occ2!=9999)  else . } %>%
  { if(vname=="occ1") filter(.,occ1!=9999)  else . } %>%
  ffl_contrib(vname,byear,"wgap") %>%
  filter(wgap == in_wg) %>%
  arrange(wgap,east,year) %>%
  select(wgap,east,year,.data[[vname]],ws,wgap_test,w,c_w,w_dfl,c_w_dfl, comp_total) %>%
  mutate(comp_dfl = w-w_dfl,
         vtype    = 1L)%>%
  group_split(wgap,east,year) %>%
  map_df(copy_tab_row,!!vname,"comp_dfl",8000L,glue("{vtitle} \n Composition Effects"),.vt=2L) %>%
  #map_df(copy_tab_row,!!vname,"comp_dfl",8001L,glue("Weighting Error"),.vt=3L) %>%
  mutate(vtype = factor(vtype, levels=c(1L,2L), 
                        labels= c("Wage Structure Effect",
                                  glue("{vtitle} \n Composition Effects"))))%>%
  select(wgap,east,year,.data[[vname]],vtype,ws) 
}

#AFunction to generate tidy effect lists for the output of FFL contributions by percentile
generate_pct_contrib<- function(.vname,
                                .comp_name,
                                .y = 1995){
  tbl_ws <- rif_pct[[.vname]] %>% 
    ffl_contrib_new(.vname,1995,"pct") %>%
    select(pct,east,year,all_of(.vname),contrib = i_ws) 
  
  tbl_comp <- rif_pct[[.vname]] %>% 
    ffl_contrib_new(.vname,1995,"pct") %>%
    group_by(pct,east,year) %>%
    slice(1) %>%
    ungroup() %>%
    transmute(pct,east,year,"{.vname}" := factor(glue("{.comp_name} Composition effects")),contrib = i_p - i_ws_total - 1) 
  
  bind_rows(tbl_ws,tbl_comp) %>%
    arrange(pct,east,year) 
}

#A Function to generate tidy effect lists for the output of FFL contributions by wage gap
generate_wgap_contrib <- function(.vname,
                                  .comp_name,
                                  .y = 1995){

  tbl_ws <-  rif_wgap[[.vname]] %>% 
    ffl_contrib_new(.vname,.y,"wgap") %>%
    select(wgap,east,year,all_of(.vname),contrib = ws) 
  
  
  tbl_comp <- rif_wgap[[.vname]] %>% 
    ffl_contrib_new(.vname,.y,"wgap") %>%
    group_by(wgap,east,year) %>%
    slice(1) %>%
    ungroup() %>%
    transmute(wgap,east,year,"{.vname}" := factor(glue("{.comp_name} Composition effects")),contrib=w-w_dfl)
  
  bind_rows(tbl_ws,tbl_comp) %>%
    arrange(wgap,east,year) 
}

#A function to clean wgap contribs for export to excel
get_wgap_export_table <- function(.tbl,.name){
  .tbl%>% 
    select(-year) %>%
    group_by(wgap,east) %>%
    mutate(overall  = sum(contrib),
           fraction =  contrib/overall) %>%
    ungroup() %>%
    pivot_wider(id_cols   =  c("wgap",.name),
                names_from = "east",
                values_from=c("contrib","overall","fraction")) %>%
    relocate(wgap,.data[[.name]],contrib_East,overall_East,fraction_East,contrib_West,overall_West,fraction_West) 
}

#A function to clean pct contribs for export to excel
get_pct_export_table <- function(.tbl,.name){
  .tbl%>% 
    select(-year) %>%
    group_by(pct,east) %>%
    mutate(overall  = sum(contrib),
           fraction =  contrib/overall) %>%
    ungroup() %>%
    pivot_wider(id_cols   =  c("pct",.name),
                names_from = "east",
                values_from=c("contrib","overall","fraction")) %>%
    relocate(pct,.data[[.name]],contrib_East,overall_East,fraction_East,contrib_West,overall_West,fraction_West) 
}




#-------------------------------------------------------------------------------------
#     6. Basic theme definitions for graphs
#-------------------------------------------------------------------------------------


theme_eddy <- function(.bs=20) {
  theme_bw(base_size = .bs) %+replace%
    theme(legend.position="bottom",
          #text = element_text(size=25),
          strip.background = element_rect(color= NA,   fill=NA),
          panel.border     = element_rect(colour = "black", fill = NA, linetype = "solid"),
          strip.text       = element_text(colour = 'black'),
          legend.title.align=0.5,
          strip.text.x = element_text(debug=FALSE,size = rel(0.8),margin = margin(3,0,3,0, "pt")),
          legend.key.width = unit(3, "line")
        ) 
}

#-------------------------------------------------------------------------------------
#     7. Samll helper functions
#-------------------------------------------------------------------------------------

map_assign <- function(x,.f,.names){
  map(x,.f = .f) %>%
    map2(.x = .names,.y=.,assign,envir=.GlobalEnv)
}
