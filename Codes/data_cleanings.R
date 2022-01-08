## code for the datas I used in the analysis

library(tidyverse)
library(WDI)
library(here)


# WorldBank ---------------------------------------------------------------

inds =
  c(
    "SH.XPD.CHEX.GD.ZS",
    "SH.XPD.CHEX.PC.CD",
    "SH.XPD.OOPC.CH.ZS",
    "SH.XPD.CHEX.PP.CD",
    "SH.XPD.GHED.PP.CD",
    "SH.XPD.GHED.PC.CD", # Domestic general government health expenditure per capita (current US$)
    "SH.XPD.PVTD.CH.ZS",
    "SH.XPD.PVTD.PP.CD",
    "SH.XPD.EHEX.PP.CD",
    "SH.XPD.EHEX.CH.ZS", # External health expenditure (% of current health expenditure)
    "SP.DYN.LE00.IN",
    "NY.GDP.PCAP.KD",
    "SP.POP.0014.TO.ZS",
    "SP.POP.65UP.TO.ZS"
  )

inds_cors = 
  c(
    "Exp_prc_Gdp",
    "Exp_pc",
    "out_of_pocket_prc_exp",
    "exp_pc_ppp",
    "Gen_Exp_ppp",
    "Gen_Exp_pc",
    "private_exp_prc",
    "private_exp_pc_ppp",
    "external_health_pc_ppp",
    "external_health_prc",
    "life_expec",
    "gdp_cp_2015",
    "pop_0_14",
    "pop_65"
  )

raw <- 
  WDI(
    country = "all",
    indicator = inds,
    end = 2020, # maalesef 2019 ve 2020 bulunmuyor
    extra = TRUE
  )

raw1 <- 
  raw %>% 
  as_tibble()

colnames(raw1)[4:17] <- inds_cors

infos <- 
  raw1 %>% 
  select(iso2c, country, iso3c:lending) %>% 
  distinct(iso2c, .keep_all = T)

data <- 
  raw1 %>% 
  select(-country, -c(iso3c:lending))

openxlsx::write.xlsx(raw1, here("data", "world_bank", "data4.xlsx"))



# OECD --------------------------------------------------------------------

data_oecd_raw <- 
  here("data", "oecd") %>% 
  list.files() %>% 
  tibble(path = .) %>% 
  mutate(datas = map(path, .f = ~read_csv(here("data", "oecd", .)))) %>% 
  unnest(cols = datas)

tmp <- 
  data_oecd_raw %>% 
  group_by(INDICATOR, SUBJECT) %>% 
  nest()

# keep info that have data for Turkey

exclude <- 
  tmp %>% 
  mutate(
    Check = map(data, 
                .f = function(x){
                  sum(x$LOCATION == "TUR")
                })
  ) %>% 
  unnest(cols = Check) %>% 
  filter(Check == 0) %>% 
  ungroup() %>% 
  distinct(INDICATOR) %>% 
  pull()

data_oecd_raw2 <- 
  data_oecd_raw %>% 
  filter(!INDICATOR %in% exclude) %>% 
  select(-`Flag Codes`) %>% 
  mutate(
    path = str_sub(
      path, start = 1, end = -5
    ) %>% 
      str_sub(
        ., start = 6
      )
  )

data_oecd <- 
  data_oecd_raw2 %>% 
  left_join(
    WDI::WDI_data$country %>% 
      as_tibble() %>% 
      select(iso3c, income, region),
    by = c("LOCATION" = "iso3c")
  )


writexl::write_xlsx(data_oecd, here("data", "final_tables", "oecd.xlsx"))


# TurkStat ----------------------------------------------------------------

raw <- read_excel("data/tuik/saglık_harcamaları.xls", col_names = F)
raw1 <- raw[-c(246,247),]

subset_fun <- function(s,e){
  raw1 <- raw1[s:e,]
  if (is.na(raw1[1,1])) {
    raw1 <- raw1[-c(1,2),]
  }
  return(raw1)
}

tidy_fun <- function(tmp){
  tmp1 <- 
    tmp %>% 
    slice(4, 6, 10, 2* c(7:15) -1) %>% 
    select(-c(...4, ...9)) 
  
  tmp1[3,2] <- "Tot_Exp"
  tmp1[4,2] <- "Current_Exp"
  tmp1[12,2] <- "Investment"
  tmp1[2,4] <- "Gen_Gov"
  tmp1[2,8] <- "Gen_Prv"
  
  tmp2 <- 
    tmp1 %>% 
    slice(-1) %>% 
    select(-...1) 
  
  tmp2[1,1] <- "Ind"
  colnames(tmp2) <-  as.character(tmp2[1,])
  tmp2 <- tmp2[-1,]
  
  tmp3 <- 
    tmp2 %>% 
    janitor::clean_names() %>% 
    mutate(across(general_total:other_1, as.numeric),
           across(general_total:other_1, 
                  .fns = ~ if_else(is.na(.), 0, .)))
  
  tmp4 <- 
    tmp3 %>% 
    pivot_longer(!ind) %>% 
    rename(detail = name) %>% 
    mutate(
      type = case_when(
        detail == "general_total" ~ "general",
        detail %in% c("gen_gov", "central_government", 
                      "local_government", 
                      "social_security_institution") ~ "gov",
        T ~ "private"
      )
    )
  return(tmp4)
  
}

data_harcama <- 
  tibble(Year = "Year", W = 22) %>% 
  uncount(weights = W) %>% 
  mutate(Year = c(1999:2020)) %>% 
  mutate(start = 42*c(0:21) + 1,
         end = 42*c(1:22),
         data = map2(start, end, subset_fun)) %>% 
  select(-c(start, end)) %>% 
  mutate(tidy_data = map(data, tidy_fun)) %>% 
  select(!data) %>% 
  unnest(cols = tidy_data)

writexl::write_xlsx(data_harcama, here("data", "tuik", "detailed_stat.xlsx"))

























