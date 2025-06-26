## Shiny App: ERA Explorer global.R
library(shiny)
library(bslib)
library(qs)
library(ggplot2)
library(dplyr)
library(lubridate)
library(DT)
library(fmsb)
library(hrbrthemes)
library(ggVennDiagram)
library(tidyr)
library(cmprsk)
library(survminer)
library(survival)
library(datasets)
library(shinymanager)
library(readr)
library(broom) 
library(shinyjs) 
library(flextable)
library(healthpopR)
library(colorspace)

# Settings ------
eval_ostpre <- TRUE        # TRUE for ostpre specific outputs / FALSE for test the program

## Credentials ----
if(file.exists("credentials/credentials.txt")) credentials <- read.table("credentials/credentials.txt", header = TRUE)
all_users <- gsub(".csv", "",  list.files("logs/", full.names = F))
# if(FALSE){
#   all_logfiles <- list.files("logs/", full.names = T)
#   all_users <- gsub(".csv", "",  list.files("logs/", full.names = F))
#   selected_logfile <- all_logfiles[1]
#   logfile <- read.csv2(selected_logfile)
# }

## Load Data & Global Variables ----
# data_codes <- arrow::read_parquet(file = paste0(loc, "data_codes.parquet"))
if(eval_ostpre == TRUE){
  loc = "data/" 
  population <- arrow::read_parquet(file = paste0(loc, "population.parquet"))
  diagnoses <- arrow::read_parquet(file = paste0(loc, "diagnoses.parquet"))
  ostpre_vastpaiv <- arrow::read_parquet(file = paste0(loc, "ostpre_vastpaiv.parquet"))
  population_variables <- arrow::read_parquet(file = paste0(loc, "edumiage.parquet"))
}else{
  loc = "data/"    # for synthetic data
  population <- arrow::read_parquet(file = paste0(loc, "synthetic_shiny/synth_population.parquet"))
  diagnoses <- arrow::read_parquet(file = paste0(loc, "synthetic_shiny/synth_diagnoses.parquet"))
  population_variables <- arrow::read_parquet(file = paste0(loc, "synthetic_shiny/synth_population_variables.parquet")) 
  ostpre_vastpaiv <- arrow::read_parquet(file = paste0(loc, "ostpre_vastpaiv.parquet")) ## TODO synthetize
}
data_censoring_date = as.Date("2023-12-31") ## Global Censoring Date Var

## Colors for App ------
colors_groups <- c(
  "non-exposure" = "#5BC0DE",
  "exposure"     = "#D9534F",
  "non-response" = "#F0AD4E",
  "response"     = "#5CB85C",
  "dead"         = "#292B2C"
)

## Registry Data Sources -----
src_choices <- unique(diagnoses$SRC)[!unique(diagnoses$SRC) %in% c("murt")] # Select globally available registry sources 
src_choices_extra <- c("murt") # Select globally available registry sources 

## Cox Model Variables ----

## Reference Values (base settings)
ref_bmi_cat1 <- levels(population_variables$bmi_cat1)
ref_bmi_cat2 <- levels(population_variables$bmi_cat2)
refs_edu <- levels(population_variables$edu)
## Cox Variables
cox_normal_vars <- c("bmi_cat1", "bmi_cat2", "edu")
cox_spline_vars <- c("age_bs", "bmi")





# Extras ----

## These need to developed into healthpopR
# group: exposure , response
tbl_pop_var <- function(data, group, var){
  ## Check that group is exposure or response
  if (!group %in% c("exposure", "response")) {
    stop("group must be either 'exposure' or 'response'")
  }
  ## Data
  if(group == "exposure"){
    tbl <- data %>% filter(exp.GROUP == "exposure") 
  }
  if(group == "response"){
    tbl <- data %>% filter(resp.GROUP == "response") 
  }
  ## Count
  tbl <- tbl %>% 
    dplyr::left_join(population_variables, by = "ID") %>% 
    count(!!sym(var), name = "n")
  return(tbl)
}


## Questionares plots 
if(TRUE){
  plot_datavaibility_all <- function(pop){
    popsize = length(unique(pop$ID[pop$exp.GROUP == "exposure"]))
    
    ostpre_vastpaiv %>%
      filter(ID %in% pop$ID[pop$exp.GROUP == "exposure"]) %>%
      pivot_longer(cols = names(ostpre_vastpaiv)[4:27], ) %>%
      filter(!is.na(value))  %>%
      group_by(name) %>%
      summarise(count = n()) %>%
      mutate(
        pct = 100 * count / popsize
      ) -> dplot
    
    quest_right_order <- c("vpvmbl","vpvm025","vpvm05","vpvm10","vpvm15","vpvm20","vpvm25","vpvm30", "vpvmcov", "vpvmfpsbl", "vpvmfps3y","vpvmkfpsbl","vpvmkfps12","vpvmkfps24" ,"vpvmkfpspt","mpvmbl","mpvm05","mpvm10","mpvm15","mpvm20","mpvm25","mpvm30","mpvmfpsbl","mpvmfps3y")
    # quest_right_order[quest_right_order %in% unique(dplot$name)]
    dplot$name <- factor(dplot$name, levels = quest_right_order[quest_right_order %in% unique(dplot$name)])
    
    ggplot(dplot) +
      # geom_bar(aes(x=reorder(name, quest_right_order[quest_right_order %in% unique(dplot$name)]), y=pct, fill = name), stat= "identity") +
      geom_bar(aes(x= name, y=pct, fill = name), stat= "identity") +
      geom_text(aes(x= name, y=pct, label = paste0(round(pct, 1), "% (", count, " answers)")), hjust = -.2) +
      hrbrthemes::theme_ipsum_rc() +
      theme(legend.position = "none") +
      labs(title = "Total Cohort", subtitle = "Number of answers in questionares" , x= "") +
      # scale_y_continuous(limits = c(0, 100)) +
      scale_y_continuous(limits = c(0, 120), breaks = c(0,25,50,75,100)) +
      coord_flip()
  }
  
  plot_datavaibility_original <- function(pop){
    
    popsize = length(unique(pop$ID[pop$exp.GROUP == "exposure" & pop$ID < 30000]))
    
    ostpre_vastpaiv %>%
      filter(ID %in% pop$ID[pop$exp.GROUP == "exposure"] & ID < 30000) %>%
      pivot_longer(cols = names(ostpre_vastpaiv)[4:27], ) %>%
      filter(!is.na(value))  %>%
      group_by(name) %>%
      summarise(count = n()) %>%
      mutate(
        pct = 100 * count / popsize
      ) -> dplot
    
    
    quest_right_order <- c("vpvmbl","vpvm025","vpvm05","vpvm10","vpvm15","vpvm20","vpvm25","vpvm30", "vpvmcov", "vpvmfpsbl", "vpvmfps3y","vpvmkfpsbl","vpvmkfps12","vpvmkfps24" ,"vpvmkfpspt","mpvmbl","mpvm05","mpvm10","mpvm15","mpvm20","mpvm25","mpvm30","mpvmfpsbl","mpvmfps3y")
    # quest_right_order[quest_right_order %in% unique(dplot$name)]
    dplot$name <- factor(dplot$name, levels = quest_right_order[quest_right_order %in% unique(dplot$name)])
    
    ggplot(dplot) +
      # geom_bar(aes(x=reorder(name, quest_right_order[quest_right_order %in% unique(dplot$name)]), y=pct, fill = name), stat= "identity") +
      geom_bar(aes(x= name, y=pct, fill = name), stat= "identity") +
      geom_text(aes(x= name, y=pct, label = paste0(round(pct, 1), "% (", count, " answers)")), hjust = -.2) +
      hrbrthemes::theme_ipsum_rc() +
      theme(legend.position = "none") +
      labs(title = "Original Cohort", subtitle = "Number of answers in questionares" , x= "") +
      scale_y_continuous(limits = c(0, 120), breaks = c(0,25,50,75,100)) +
      coord_flip()
    
  }
  
  plot_datavaibility_extra <- function(pop){
    
    popsize = length(unique(pop$ID[pop$exp.GROUP == "exposure" & pop$ID >= 30000]))
    
    ostpre_vastpaiv %>%
      filter(ID %in% pop$ID[pop$exp.GROUP == "exposure"] & ID >= 30000) %>%
      pivot_longer(cols = names(ostpre_vastpaiv)[4:27], ) %>%
      filter(!is.na(value))  %>%
      group_by(name) %>%
      summarise(count = n()) %>%
      mutate(
        pct = 100 * count / popsize
      ) -> dplot
    
    quest_right_order <- c("vpvmbl","vpvm025","vpvm05","vpvm10","vpvm15","vpvm20","vpvm25","vpvm30", "vpvmcov", "vpvmfpsbl", "vpvmfps3y","vpvmkfpsbl","vpvmkfps12","vpvmkfps24" ,"vpvmkfpspt","mpvmbl","mpvm05","mpvm10","mpvm15","mpvm20","mpvm25","mpvm30","mpvmfpsbl","mpvmfps3y")
    # quest_right_order[quest_right_order %in% unique(dplot$name)]
    dplot$name <- factor(dplot$name, levels = quest_right_order[quest_right_order %in% unique(dplot$name)])
    
    ggplot(dplot) +
      # geom_bar(aes(x=reorder(name, quest_right_order[quest_right_order %in% unique(dplot$name)]), y=pct, fill = name), stat= "identity") +
      geom_bar(aes(x= name, y=pct, fill = name), stat= "identity") +
      geom_text(aes(x= name, y=pct, label = paste0(round(pct, 1), "% (", count, " answers)")), hjust = -.2) +
      hrbrthemes::theme_ipsum_rc() +
      theme(legend.position = "none") +
      labs(title = "Extra Cohort", subtitle = "Number of answers in questionares" , x= "") +
      scale_y_continuous(limits = c(0, 120), breaks = c(0,25,50,75,100)) +
      coord_flip()
    
  }
  
}