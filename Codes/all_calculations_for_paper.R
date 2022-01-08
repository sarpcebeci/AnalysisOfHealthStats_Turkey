## Code for the Plots that I generated
library(fpp3)
library(tidyverse)
library(readxl)
library(here)
library(fixest)


# Plots: TurkStat ---------------------------------------------------------

data <- read_excel("data/tuik/detailed_stat.xlsx") %>% 
  filter(Year > 2000)

ind_wanted <- 
  data %>% 
  distinct(ind) %>% 
  filter(!ind %in% c("Tot_Exp", "Current_Exp")) %>% 
  pull()

library(rvest)

cpi_url <- "https://www.tcmb.gov.tr/wps/wcm/connect/EN/TCMB+EN/Main+Menu/Statistics/Inflation+Data/Consumer+Prices"

raw_list <- 
  cpi_url %>% 
  read_html() %>% 
  html_table()

cpi_tbl <- 
  raw_list[[1]] %>% 
  bind_rows(raw_list[[2]]) %>% 
  janitor::clean_names() %>% 
  rename(date = x1) %>% 
  mutate(
    date = str_c("01-", date) %>% 
      lubridate::dmy(),
    mnt = lubridate::month(date),
    yr = lubridate::year(date)
  ) %>% 
  filter(mnt == 12, yr > 2000) %>% 
  select(yr, cpi_change = cpi_year_to_year_percent_changes) %>% 
  arrange(yr) %>% 
  mutate(cpi_change = cpi_change / 100 + 1,
         cpi_change = if_else(yr == 2001, 1, cpi_change))

cpi_tbl$cpi = 1

for (i in c(2:nrow(cpi_tbl))) {
  cpi_tbl[i,3] = cpi_tbl[(i-1),3] * cpi_tbl[i,2]
}

cpi_tbl$cpi_change = NULL


vis_fun <- function(tbl){ 
  
  tbl_plt <- 
    tbl %>% 
    mutate(type = if_else(detail == "gen_gov", "gen_gov", type),
           type = if_else(detail == "gen_prv", "gen_prv", type)) %>% 
    filter(type %in% c("gov", "private")) 
  
  
  tbl_plt %>% 
    ggplot(aes(Year, value, color = detail)) +
    geom_point() + 
    geom_line() + 
    labs(
      y = "in million 2001 TRY",
      x = "", 
      title = str_c("Expenditure of ", tbl_plt$ind1[1]),
      color = "Institution:"
    )
}

plt_tbl <- 
  data %>% 
  left_join(cpi_tbl, by = c("Year" = "yr")) %>% 
  mutate(value = value / cpi,
         ind1 = ind,
         value = if_else(ind == "Nursing and residential care facilities",
                         1, value)) %>% 
  group_by(ind) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(data = map(data, vis_fun),
         ind = str_replace_all(ind, " ", "_"))

map2(
  plt_tbl$ind,
  plt_tbl$data,
  .f = function(pth, plt){
    ggsave(plot = plt,
           filename = here("plots", "tuik",
                           str_c(pth, ".png")))
  }
)


# Plots: OECD -------------------------------------------------------------

data <- 
  read_excel(
    here("data", "final_tables", "oecd.xlsx")
  ) %>% 
  janitor::clean_names()

vis_plottr <- function(tbl){  
  tbl %>% 
    filter(location == "TUR") %>% 
    ggplot(aes(time, value)) +
    geom_line(color = "red") +
    labs(
      title = str_c(str_to_title(tbl$path[1]), " of Turkey", " in ", tbl$subject1[1]),
      y = tbl$measure1[1],
      x = ""
    )
}

vis_cmp_cnt <- function(tbl){ 
  tbl %>% 
    mutate(clr = if_else(location == "TUR", "Turkey", "Other Countries")) %>% 
    ggplot(aes(time, value, group = location)) +
    geom_line(aes(color = clr)) +
    labs(
      title = str_c(str_to_title(tbl$path[1]), " of Turkey", " in ", tbl$subject1[1]),
      y = tbl$measure1[1],
      x = "",
      color = ""
    ) 
}

vis_cmp_grp <- function(tbl){ 
  tbl %>% 
    mutate(
      income = if_else(location == "TUR", "Turkey", income),
      region = if_else(location == "TUR", "Turkey", region)
    ) %>% 
    pivot_longer(cols = c(income, region), 
                 names_to = "comparison_group",
                 values_to = "comparison_value") %>% 
    ggplot(aes(time, value, color = comparison_value)) +
    geom_smooth(se = F, method = "loess", formula = "y ~ x") +
    facet_wrap(vars(comparison_group)) +
    labs(
      title = str_c(str_to_title(tbl$path[1]), " Comparisons", " in ", tbl$subject1[1]),
      subtitle = "Across income groups and regions",
      y = tbl$measure1[1],
      x = "",
      color = ""
    )
}

tbl_plots <- data %>% 
  mutate(subject1 = subject, measure1 = measure) %>% 
  group_by(indicator, subject, measure) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(plttr = map(data, vis_plottr),
         cmp_grp = map(data, vis_cmp_grp),
         cmp_cnt = map(data, vis_cmp_cnt),
         paths = str_c(tolower(indicator), "_", 
                       tolower(subject), "_",
                       tolower(measure))) 

map2(
  tbl_plots$paths,
  tbl_plots$plttr,
  .f = function(pth, plt){
    ggsave(
      plot = plt,
      here("plots", "oecd", str_c(pth, "_plttr.png"))
    )
  }
)

map2(
  tbl_plots$paths,
  tbl_plots$cmp_grp,
  .f = function(pth, plt){
    ggsave(
      plot = plt,
      here("plots", "oecd", str_c(pth, "_cmp_grp.png"))
    )
  }
)

map2(
  tbl_plots$paths,
  tbl_plots$cmp_cnt,
  .f = function(pth, plt){
    ggsave(
      plot = plt,
      here("plots", "oecd", str_c(pth, "_cmp_cnt.png"))
    )
  }
)


# Plots: WorldBank --------------------------------------------------------

data <- 
  read_excel(
    here("data", "world_bank", "data4.xlsx")
  ) %>% 
  select(
    -c(capital:lending)
  )

data_long <- 
  data %>% 
  pivot_longer(
    cols = c(Exp_prc_Gdp:prc_yng_pop),
    names_to = "variables",
    values_to = "value"
  )

data_title <- 
  data_long %>% 
  distinct(variables) %>% 
  mutate(
    var_title = c(
      "Current health expenditure (% of GDP)",
      "Current health expenditure per capita (current US$)",
      "Out-of-pocket expenditure (% of current health expenditure)",
      "Current health expenditure per capita, PPP (current international $)",
      "Domestic general government health expenditure per capita, PPP (current international $)",
      "Domestic general government health expenditure per capita (current US$)",
      "Domestic private health expenditure (% of current health expenditure)",
      "Domestic private health expenditure per capita, PPP (current international $)",
      "External health expenditure per capita, PPP (current international $)",
      "External health expenditure (% of current health expenditure)",
      "Life Expectancy at Birth",
      "GDP per capita in 2015 US$",
      "Population ages 0-14 (% of total population)"
    )
  )

data_long <- 
  data_long %>% 
  left_join(data_title, by = "variables")


compare_grps <- 
  c(
    "Central Europe and the Baltics",
    "Latin America & the Caribbean (IDA & IBRD countries)",
    "Middle East & North Africa (IDA & IBRD countries)",
    "High income", "Low income", "Lower middle income", 
    "Low & middle income", "Middle income", 
    "Middle East & North Africa (excluding high income)",
    "Upper middle income", "Turkey"
  )

plot_tr <- function(tbl){
  tbl_tr <- 
    tbl %>% 
    filter(
      iso2c == "TR"
    ) 
  
  tbl_tr %>% 
    ggplot(aes(year, value)) +
    geom_line() +
    labs(
      title = tbl$var_title[1],
      subtitle = "For Turkey",
      x = "", y = ""
    )
}

plot_cmp <- function(tbl){
  data_long_agg <- 
    tbl %>% 
    filter(country %in% compare_grps)
  
  tbl_cmp <- 
    data_long_agg %>% 
    mutate(alp = if_else(country == "Turkey",
                         1, .7),
           ltp = if_else(country == "Turkey",
                         "g1", "g2")) 
  
  tbl_cmp %>%
    mutate(country = fct_reorder(country, desc(value))) %>% 
    ggplot(aes(year, value)) +
    geom_line(aes(color = country, alpha = alp, linetype = ltp), size = 1.5) +
    scale_alpha_continuous(range = c(0.3, 1), guide = "none") +
    scale_linetype_discrete(guide = "none") +
    labs(
      title = tbl_cmp$var_title[1],
      color = "", x = "", y = ""
    )
}



plt_tbl <- 
  data_long %>% 
  group_by(variables) %>% 
  nest() %>% 
  mutate(
    plt_tr = map(data, plot_tr),
    plt_cmp = map(data, plot_cmp)
  )


map2(
  plt_tbl$variables,
  plt_tbl$plt_tr,
  .f = function(pth, plt){
    ggsave(plot = plt, here("plots", "wb", str_c(pth, "_tr.png")))
  }
)

map2(
  plt_tbl$variables,
  plt_tbl$plt_cmp,
  .f = function(pth, plt){
    ggsave(plot = plt, here("plots", "wb", str_c(pth, "_cmp.png")))
  }
)  


# Estimation: Health Spendings Turkey -------------------------------------

data <- read_excel("data/tuik/detailed_stat.xlsx") %>% 
  filter(Year > 2000)

exp_pop <- 
  WDI(country = "TR", 
      indicator = c("SP.DYN.LE00.IN", "SP.POP.TOTL")) %>% 
  as_tibble() %>% 
  rename(life_expec = SP.DYN.LE00.IN,
         pop = SP.POP.TOTL)

analysis_table <- 
  data %>% 
  left_join(cpi_tbl, by = c("Year" = "yr")) %>% 
  mutate(value = value / cpi) %>% 
  filter(ind == "Tot_Exp",
         detail == "general_total") %>% 
  select(year = Year,
         spending = value) %>% 
  left_join(exp_pop, "year") %>% 
  mutate(
    life_expec = if_else(
      is.na(life_expec), 77.7, life_expec),
    lpop = log(pop),
    llife = log(life_expec),
    lspending = log(spending)) %>% 
  as_tsibble(index = "year")

fit_arima <- 
  analysis_table %>% 
  model(ARIMA(spending))

report(fit_arima)

forecasts <- 
  fit_arima %>% 
  forecast(h = 10) 

forecasts %>% 
  autoplot(analysis_table) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(y = "in thousand TRY")

fit_arima %>% 
  gg_tsresiduals()


# Estimation: Senescent Population ----------------------------------------

analysis_table2 <- 
  read_excel(
    here("data", "world_bank", "data4.xlsx")
  ) %>% 
  select(
    -c(capital:lending)
  ) %>% 
  select(iso2c, country, region, year, Exp_pc,
         gdp_cp_2015, pop_0_14, life_expec,
         pop_65) %>% 
  filter(region != "Aggregates") %>% 
  na.omit() %>% 
  add_count(country) %>% 
  filter(n == 19) %>% 
  select(-n) %>% 
  left_join(
    WDI::WDI_data$country %>% 
      as_tibble() %>% 
      select(iso2c, income),
    by = "iso2c"
  ) %>% 
  mutate(
    mid = if_else(
      income %in% c("Upper middle income", "Lower middle income"),
      1,0
    )
  )

model_main <- 
  feols(
    data = analysis_table2 %>% 
      filter(income %in% 
               c("Upper middle income", "Lower middle income")),
    fml = Exp_pc ~ pop_65 + gdp_cp_2015  | country + year,
    cluster = ~country
  )

model_all_cntr <- 
  feols(
    data = analysis_table2 ,
    fml = Exp_pc ~ pop_65 + gdp_cp_2015  | country + year,
    cluster = ~country
  )


# Estimation: Population --------------------------------------------------

data <- 
  read_excel(
    here("data", "world_bank", "data4.xlsx")
  ) %>% 
  select(
    -c(capital:lending)
  )

tr_pop <- 
  data %>% 
  filter(country == "Turkey") %>% 
  select(year, pop_65) %>% 
  as_tsibble(index = year)

fit_arima <- 
  tr_pop %>% 
  model(ARIMA(pop_65))

report(fit_arima)

forecasts <- 
  fit_arima %>% 
  forecast(h = 10) 

forecasts %>% autoplot()




















