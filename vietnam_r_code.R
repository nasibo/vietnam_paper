library(tidyverse)
library(plm)
library(writexl)
library(lmtest)
library(cAIC4)
library(plm)
library(quantreg)
library(rqpd)
library(uqr)
library(stargazer)

## set working directory
setwd("your_working_directory")

########################################################
## IMPORT DATA
########################################################

## PAPI data
data_2011 <- read.csv("papi_2011.csv")
names_2011 <- names(data_2011)

papi <- NULL
years <- 2011:2020
for(i in 1:length(years)){
  year <- years[i]
  file_name <- paste("papi_", year, ".csv", sep = "")
  year_data <- read.csv(file_name)
  year_data <- year_data %>% select(any_of(names_2011)) 
  year_data$year <- year
  papi <- rbind(papi, year_data)
}

papi <- papi %>% 
  rename(province = Provinces)

## wealth inequality data
income <- read.csv("vietnam_prov_quin_income.csv")

## inflation data 
cpi <- read.csv("world_bank_vietnam_cpi.csv")

## teachers 
teachers <- read.csv("teachers.csv")

## doctors
doctors <- read.csv("doctors.csv")

## percent of provincial population that is minority
minority <- read.csv("minority.csv")

## provincial urban population 
urban_pop <- read.csv("urban_pop.csv")

## provincial total population 
total_pop <- read.csv("total_pop.csv")

## population density
density <- read.csv("pop_dense.csv")

########################################################
## clean and transform data when needed
########################################################

# start with dependent variable.
# transform wealth data into 2010 VND using CPI inflator
income_constant <- left_join(income, cpi, by = "year")

income_constant <- income_constant %>%
  group_by(year) %>%
  mutate(q1 = q1 * (1/(cpi/100)),
         q2 = q2 * (1/(cpi/100)),
         q3 = q3 * (1/(cpi/100)),
         q4 = q4 * (1/(cpi/100)),
         q5 = q5 * (1/(cpi/100)))

# standardize province names
papi[papi$province == "TT-Hue", "province"] <- "Thua Thien-Hue"

## transform population density data into long data frame
density <- density %>%
  pivot_longer(!province, names_to = "year", values_to = "density")

density$year <- as.numeric(gsub("[^0-9.-]", "", density$year))

## transform total population data into long data frame
total_pop <- total_pop %>%
  pivot_longer(!province, names_to = "year", values_to = "total_pop")

total_pop$year <- as.numeric(gsub("[^0-9.-]", "", total_pop$year))

## transform urban population data into long data frame
urban_pop <- urban_pop %>%
  pivot_longer(!province, names_to = "year", values_to = "urban_pop")

urban_pop$year <- as.numeric(gsub("[^0-9.-]", "", urban_pop$year))

## transform doctors data into long data frame
doctors <- doctors %>%
  pivot_longer(!province, names_to = "year", values_to = "doctors")

doctors$year <- as.numeric(gsub("[^0-9.-]", "", doctors$year))

## transform teachers data into long data frame
teachers <- teachers %>%
  pivot_longer(!province, names_to = "year", values_to = "teachers")

teachers$year <- as.numeric(gsub("[^0-9.-]", "", teachers$year))


########################################################
## create new variables or variable names
########################################################

# create dependent variable
income_constant$inequality <- income_constant$q5/income_constant$q1


# create percent of urban population
urban_share <- left_join(total_pop, urban_pop, by = c("province", "year"))
urban_share$urban_share <- 100 * (urban_share$urban_pop/urban_share$total_pop)

# code PAPI names
papi_names <- colnames(papi)
papi_codes <- data.frame(papi_full_names = papi_names)
papi_codes <- papi_codes %>%
  mutate(papi_code = gsub("[[:punct:]]", "", papi_names)) %>%
  mutate(papi_code = gsub("[[:alpha:]]", "", papi_code)) %>%
  mutate(papi_code = paste("p", papi_code, sep = "" ))
papi_codes[papi_codes$papi_full_names == "province", "papi_code"] <- "province"
papi_codes[papi_codes$papi_full_names == "year", "papi_code"] <- "year"

colnames(papi) <- papi_codes$papi_code

# create composite PAPI score
papi$score <- papi$p1 + papi$p2 + papi$p3 + papi$p4 + papi$p5 + papi$p6

# create a PAPI data set of the main PAPI variables lagged by one year.
# e.g. 2015 PAPI data will be joined with 2016 wealth data.
papi_1_lag <- papi %>%
  select(province, year, score, p1, p2, p3, p4, p5, p6) %>%
  mutate(year = year + 1) %>%
  rename(scorel = score, 
         p1l = p1, p2l = p2, p3l = p3, p4l = p4, p5l = p5, p6l = p6)

########################################################
## construct panel data dataframe for regression
########################################################
#data <- income_constant %>%
#  select(province, year, inequality)
data <- income_constant
data <- left_join(data, minority, by = "province")
data <- left_join(data, density, by = c("province", "year"))
data <- left_join(data, urban_share, by = c("province", "year"))
data <- left_join(data, teachers, by = c("province", "year"))
data <- left_join(data, doctors, by = c("province", "year"))
data <- left_join(data, papi, by = c("province","year"))
data <- left_join(data, papi_1_lag, by = c("province","year"))

## remove 2010 because there is no PAPI data for that year
data <- data %>%
  filter(year != 2010)

# make year and province factor variables
data$province <- as.factor(data$province)
data$year <- as.factor(as.character(data$year))

########################################################
## visually analyze data 
########################################################

# Q: plot the inequality over time to understand variability of 
# data, that is, is it heteroscedastic over time?
ggplot(data = data, aes(x = year, y = inequality)) + 
  geom_point() 
# A: the data appears to be homoscedastic, however the preliminary 
# data from 2020 is a clear outlier, indicating this preliminary 
# data should not be in the model. After the General Statistics Office 
# of Vietnam finalizes 2020 data that could be used. 

## take out year 2020 data 
data <- data %>% 
  filter(year != 2020)

########################################################
## create simple quantile regression plots to explore the data
########################################################

## plot the quantile regression of inequality on the aggregate PAPI score
ggplot(data = data, aes(x = scorel, y = inequality)) + 
  geom_point(stroke = 0, alpha = .3, shape = 16, size = 3) +
  geom_quantile(quantiles = c(.25, .5, .75)) + 
  theme_classic() + 
  ylab("Ratio of 5th to 1st income quintile") + 
  xlab("One-year lagged value of PAPI") + 
  ggtitle("Regression of inequality on aggregate PAPI score \n at the 25th, 50th, and 75th conditional quantiles") + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave("output_plots/ineq_papi_total.png", width = 17, height = 8, units = "cm")

## plot the quantile regression of inequality on the six PAPI scores
# First structure the data correctly

plot_data <- data %>%
  select(province, year, inequality, p1l, p2l, p3l, p4l, p5l, p6l) %>%
  pivot_longer(cols = 4:9, names_to = "dimension")

plot_data[plot_data$dimension=="p1l", "dimension"] <- "Participation"
plot_data[plot_data$dimension=="p2l", "dimension"] <- "Transparency"
plot_data[plot_data$dimension=="p3l", "dimension"] <- "Vertical accountability"
plot_data[plot_data$dimension=="p4l", "dimension"] <- "Control of corruption"
plot_data[plot_data$dimension=="p5l", "dimension"] <- "Public administrative procedures"
plot_data[plot_data$dimension=="p6l", "dimension"] <- "Public service delivery"

ggplot(data = plot_data, aes(x = value, y = inequality)) + 
  geom_point(stroke = 0, alpha = .3, shape = 16) + 
  facet_wrap(~ dimension, ncol = 2) +
  geom_quantile(quantiles = c(.25, .5, .75)) + 
  theme_classic() + 
  ylab("Ratio of 5th to 1st income quintile") + 
  xlab("One-year lagged value of PAPI") + 
  ggtitle("Regression of inequality on six PAPI scores \n at the 25th, 50th, and 75th conditional quantiles") + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave("output_plots/ineq_papi_6.png", width = 17, units = "cm")

########################################################
## CQR model on aggregate PAPI score
########################################################

#########
# reconstruct the Nguyen model using Canay's method
#########
# 1) estimate the provincial dummy effect
prov_dummy <- lm(log(inequality) ~ log(scorel) +
  log(density) + urban_share + log(teachers) + log(doctors) +
  year + province - 1, 
  data = data) 
summary(prov_dummy)

## get column of provincial dummy effect and subtract from
## original logged inequality (dependent variable)
prov_dummy <- data.frame(coef = coef(prov_dummy))
prov_dummy$province <- rownames(prov_dummy)
rownames(prov_dummy) <- NULL
prov_dummy <- prov_dummy[str_detect(prov_dummy$province, "province"),]
prov_dummy <- prov_dummy %>%
  mutate(province = str_sub(province, start = 9)) 

# add on the reference province
angiang <- data.frame(coef = 0, province = "An Giang")
prov_dummy <- rbind(angiang, prov_dummy)

#make new dependent variable
orig_log_ineq <- data %>%
  select(province, year, inequality) %>%
  mutate(ln_inequality = log(inequality))

orig_log_ineq <- left_join(orig_log_ineq, prov_dummy, by = "province")

orig_log_ineq <- orig_log_ineq %>%
  mutate(inequality_ln_dif = ln_inequality - coef) %>%
  select(province, year, inequality_ln_dif)

data <- left_join(data, orig_log_ineq, by = c("province", "year"))

#2) run the quantile regression on 

# make the vector of taus (quantiles)
quantiles <- c(.25, .50, .75)

# specify the formula
cqr_formula <- inequality_ln_dif ~ log(scorel) +
  log(density) + urban_share + log(teachers) + log(doctors) + year

# run CQR for each quantile of interest. Note, running the regression for 
# each quantile isn't elegant code, but it makes creating the output table 
# much easier. 
cqr_25 <- rq(cqr_formula, data = data, tau = .25)
cqr_50 <- rq(cqr_formula, data = data, tau = .50)
cqr_75 <- rq(cqr_formula, data = data, tau = .75)

## create a table of results
stargazer(cqr_25, cqr_50, cqr_75,
  title = "CQR results", type = "html", rq.se = "boot", 
  dep.var.labels = "Log of 5th to 1st income quintile", 
  column.labels = c("25th quantile", "50th quantile", "75th quantile"), 
  covariate.labels=c("Log of lagged PAPI","Log of population density", 
    "Share of urban population", "Log of teachers", "Log of doctors",
    "2014 dummy", "2016 dummy", "2018 dummy", "2019 dummy", "Constant"), 
  model.numbers=FALSE, out = "cqr_results.html")


########################################################
## CQR model on six PAPI scores
########################################################

## create for loop to loop over each of the 6 PAPI scores
papi_list <- c("p1l", "p2l", "p3l", "p4l", "p5l", "p6l")
cqr_formulas <- NULL
for(i in 1:length(papi_list)){
  
  #########
  # reconstruct the Nguyen model using Canay's method
  #########
  # 1) estimate the provincial dummy effect
  dep_var <- papi_list[i]
  
  mod_formula <- paste("log(inequality)", "~", "log(", papi_list[i], ")",
    "+ log(density) + urban_share + log(teachers) + log(doctors) + year + province - 1", sep = "")
  
  prov_dummy <- lm(mod_formula, 
                   data = data) 
  
  summary(prov_dummy)

  ## get column of provincial dummy effect and subtract from
  ## original logged inequality (dependent variable)
  prov_dummy <- data.frame(coef = coef(prov_dummy))
  prov_dummy$province <- rownames(prov_dummy)
  rownames(prov_dummy) <- NULL
  prov_dummy <- prov_dummy[str_detect(prov_dummy$province, "province"),]
  prov_dummy <- prov_dummy %>%
    mutate(province = str_sub(province, start = 9)) 
  
  # add on the reference province
  angiang <- data.frame(coef = 0, province = "An Giang")
  prov_dummy <- rbind(angiang, prov_dummy)
  
  #make new dependent variable
  orig_log_ineq <- data %>%
    select(province, year, inequality) %>%
    mutate(ln_inequality = log(inequality))
  
  orig_log_ineq <- left_join(orig_log_ineq, prov_dummy, by = "province")
  
  orig_log_ineq <- orig_log_ineq %>%
    mutate(inequality_ln_dif = ln_inequality - coef) %>%
    select(province, year, inequality_ln_dif)
  
  data_cqr <- left_join(data, orig_log_ineq, by = c("province", "year"))
  
  assign(paste0("data_cqr", i), data_cqr)
  
  #2) get quantile regression forumla 
  
  # specify the formula
  cqr_formula <- paste("inequality_ln_dif", "~", "log(", papi_list[i], ")", 
    "+ log(density) + urban_share + log(teachers) + log(doctors) + year")
  
  cqr_formulas <- c(cqr_formulas ,cqr_formula)
  
}  

# run CQR for each quantile of interest. Note, running the regression for 
# each quantile isn't elegant code, but it makes creating the output table 
# much easier. 
  cqr_25_p1l <- rq(cqr_formulas[1], data = data_cqr1, tau = .25)
  cqr_50_p1l <- rq(cqr_formulas[1], data = data_cqr1, tau = .50)
  cqr_75_p1l <- rq(cqr_formulas[1], data = data_cqr1, tau = .75)
  
  cqr_25_p2l <- rq(cqr_formulas[2], data = data_cqr2, tau = .25)
  cqr_50_p2l <- rq(cqr_formulas[2], data = data_cqr2, tau = .50)
  cqr_75_p2l <- rq(cqr_formulas[2], data = data_cqr2, tau = .75)
  
  cqr_25_p3l <- rq(cqr_formulas[3], data = data_cqr3, tau = .25)
  cqr_50_p3l <- rq(cqr_formulas[3], data = data_cqr3, tau = .50)
  cqr_75_p3l <- rq(cqr_formulas[3], data = data_cqr3, tau = .75)
  
  cqr_25_p4l <- rq(cqr_formulas[4], data = data_cqr4, tau = .25)
  cqr_50_p4l <- rq(cqr_formulas[4], data = data_cqr4, tau = .50)
  cqr_75_p4l <- rq(cqr_formulas[4], data = data_cqr4, tau = .75)
  
  cqr_25_p5l <- rq(cqr_formulas[5], data = data_cqr5, tau = .25)
  cqr_50_p5l <- rq(cqr_formulas[5], data = data_cqr5, tau = .50)
  cqr_75_p5l <- rq(cqr_formulas[5], data = data_cqr5, tau = .75)
  
  cqr_25_p6l <- rq(cqr_formulas[6], data = data_cqr6, tau = .25)
  cqr_50_p6l <- rq(cqr_formulas[6], data = data_cqr6, tau = .50)
  cqr_75_p6l <- rq(cqr_formulas[6], data = data_cqr6, tau = .75)
  
  
# create output table  
stargazer(cqr_25_p1l, cqr_50_p1l, cqr_75_p1l, cqr_25_p2l, cqr_50_p2l, cqr_75_p2l,
  cqr_25_p3l, cqr_50_p3l, cqr_75_p3l, cqr_25_p4l, cqr_50_p4l, cqr_75_p4l,
  cqr_25_p5l, cqr_50_p5l, cqr_75_p5l, cqr_25_p6l, cqr_50_p6l, cqr_75_p6l,
  title = "CQR results of each PAPI dimension", type = "html", rq.se = "boot", 
  omit = c("year2014", "year2016", "year2018", "year2019"),
  dep.var.labels = "Log of 5th to 1st income quintile",
  column.labels = c("25th quantile", "50th quantile", "75th quantile",
                    "25th quantile", "50th quantile", "75th quantile",
                    "25th quantile", "50th quantile", "75th quantile",
                    "25th quantile", "50th quantile", "75th quantile",
                    "25th quantile", "50th quantile", "75th quantile",
                    "25th quantile", "50th quantile", "75th quantile"),
  covariate.labels=c("Log of lagged score of participation at local levels",
    "Log of lagged score of transparency in local policymaking",
    "Log of lagged score of vertical accountability",
    "Log of lagged score of control of corruption in the public sector",
    "Log of lagged score of public administration procedures",
    "Log of lagged score of public serviec delivery",
    "Log of population density", 
    "Share of urban population", "Log of teachers", "Log of doctors",
    "Constant"),
  model.numbers=FALSE,
  out = "cqr_results_6.html")

#############################################################
# Run the unconditional quantile regression on aggregate PAPI
#############################################################

## obtain the data needed and put it into a data frame object
uqr_mod_data <- data %>%
  mutate(inequality_ln = log(inequality), 
         scorel_ln = log(scorel), 
         density_ln = log(density), 
         teachers_ln = log(teachers), 
         doctors_ln = log(doctors)) %>%
  select(province, year,inequality_ln, scorel_ln, density_ln, urban_share,
         teachers_ln, doctors_ln)
uqr_mod_data <- as.data.frame(uqr_mod_data)
uqr_mod_data$province <- as.factor(uqr_mod_data$province)

# run the UQR
quantiles <- c(.25, .50, .75)
uqr_formula <- log(inequality) ~ log(scorel) +
  log(density) + urban_share + log(teachers) + log(doctors)
cre_agg <- ~ log(scorel) + log(density) + urban_share + log(teachers) + log(doctors)

uqr_formula <- inequality_ln ~ scorel_ln +
  density_ln + urban_share + teachers_ln + doctors_ln
cre_agg <- ~ scorel_ln + density_ln + urban_share + teachers_ln + doctors_ln

uqr_mod <- urq(uqr_formula, 
    data = uqr_mod_data, 
    tau = quantiles,
    cre = cre_agg, 
    id = "province")

# export the summary statistics to make into a table in Excel since 
# Stargazer doesn't recognize urq type objects :(
uqr_agg_results <- urqCI(uqr_mod, seed = 777, cluster = "province")
uqr_agg_results_names <- data.frame(name = rownames(uqr_agg_results$results))
uqr_agg_results <- cbind(uqr_agg_results_names, as.data.frame(uqr_agg_results$results))
rownames(uqr_agg_results) <- NULL
write_xlsx(uqr_agg_results, "output_data/uqr_agg_pap.xlsx")

#############################################################
# Run the unconditional quantile regression the dimensions of PAPI
#############################################################

## obtain the data needed and put it into a data frame object
uqr_mod_data <- data %>%
  mutate(inequality_ln = log(inequality),
         p1l_ln = log(p1l),
         p2l_ln = log(p2l),
         p3l_ln = log(p3l),
         p4l_ln = log(p4l),
         p5l_ln = log(p5l),
         p6l_ln = log(p6l),
         scorel_ln = log(scorel), 
         density_ln = log(density), 
         teachers_ln = log(teachers), 
         doctors_ln = log(doctors)) %>%
  select(province, year, inequality_ln, p1l_ln, p2l_ln, p3l_ln, p4l_ln, p5l_ln, p6l_ln, 
         scorel_ln, density_ln, urban_share, teachers_ln, doctors_ln)
uqr_mod_data <- as.data.frame(uqr_mod_data)
uqr_mod_data$province <- as.factor(uqr_mod_data$province)

## write the formulas
uqr_p1 <- inequality_ln ~ p1l_ln +
  density_ln + urban_share + teachers_ln + doctors_ln
cre_p1 <- ~ p1l_ln + density_ln + urban_share + teachers_ln + doctors_ln

uqr_p2 <- inequality_ln ~ p2l_ln +
  density_ln + urban_share + teachers_ln + doctors_ln
cre_p2 <- ~ p2l_ln + density_ln + urban_share + teachers_ln + doctors_ln

uqr_p3 <- inequality_ln ~ p3l_ln +
  density_ln + urban_share + teachers_ln + doctors_ln
cre_p3 <- ~ p3l_ln + density_ln + urban_share + teachers_ln + doctors_ln

uqr_p4 <- inequality_ln ~ p4l_ln +
  density_ln + urban_share + teachers_ln + doctors_ln
cre_p4 <- ~ p4l_ln + density_ln + urban_share + teachers_ln + doctors_ln

uqr_p5 <- inequality_ln ~ p5l_ln +
  density_ln + urban_share + teachers_ln + doctors_ln
cre_p5 <- ~ p5l_ln + density_ln + urban_share + teachers_ln + doctors_ln

uqr_p6 <- inequality_ln ~ p6l_ln +
  density_ln + urban_share + teachers_ln + doctors_ln
cre_p6 <- ~ p6l_ln + density_ln + urban_share + teachers_ln + doctors_ln

# run the UQR models
uqr_mod_p1 <- urq(uqr_p1, data = uqr_mod_data, tau = quantiles,
                  cre = cre_p1, id = "province")
uqr_mod_p2 <- urq(uqr_p2, data = uqr_mod_data, tau = quantiles,
                  cre = cre_p2, id = "province")
uqr_mod_p3 <- urq(uqr_p3, data = uqr_mod_data, tau = quantiles,
                  cre = cre_p3, id = "province")
uqr_mod_p4 <- urq(uqr_p4, data = uqr_mod_data, tau = quantiles,
                  cre = cre_p4, id = "province")
uqr_mod_p5 <- urq(uqr_p5, data = uqr_mod_data, tau = quantiles,
                  cre = cre_p5, id = "province")
uqr_mod_p6 <- urq(uqr_p6, data = uqr_mod_data, tau = quantiles,
                  cre = cre_p6, id = "province")

# export the summary statistics to make into a table in Excel since 
# Stargazer doesn't recognize urq type objects :(
# Note: The code below should be in a loop but I was pressed for time. 
uqr_p1_results <- urqCI(uqr_mod_p1, seed = 777, cluster = "province")
uqr_p1_results_names <- data.frame(name = rownames(uqr_p1_results$results))
uqr_p1_results <- cbind(uqr_p1_results_names, as.data.frame(uqr_p1_results$results))
rownames(uqr_p1_results) <- NULL
uqr_p1_results <- uqr_p1_results %>%
  select(name, Coef, `P>|t|`, `Std.Err.`,tau) 
uqr_p1_results$var <- "p1"

uqr_p2_results <- urqCI(uqr_mod_p2, seed = 777, cluster = "province")
uqr_p2_results_names <- data.frame(name = rownames(uqr_p2_results$results))
uqr_p2_results <- cbind(uqr_p2_results_names, as.data.frame(uqr_p2_results$results))
rownames(uqr_p2_results) <- NULL
uqr_p2_results <- uqr_p2_results %>%
  select(name, Coef, `P>|t|`, `Std.Err.`,tau) 
uqr_p2_results$var <- "p2"

uqr_p3_results <- urqCI(uqr_mod_p3, seed = 777, cluster = "province")
uqr_p3_results_names <- data.frame(name = rownames(uqr_p3_results$results))
uqr_p3_results <- cbind(uqr_p3_results_names, as.data.frame(uqr_p3_results$results))
rownames(uqr_p3_results) <- NULL
uqr_p3_results <- uqr_p3_results %>%
  select(name, Coef, `P>|t|`, `Std.Err.`,tau) 
uqr_p3_results$var <- "p3"

uqr_p4_results <- urqCI(uqr_mod_p4, seed = 777, cluster = "province")
uqr_p4_results_names <- data.frame(name = rownames(uqr_p4_results$results))
uqr_p4_results <- cbind(uqr_p4_results_names, as.data.frame(uqr_p4_results$results))
rownames(uqr_p4_results) <- NULL
uqr_p4_results <- uqr_p4_results %>%
  select(name, Coef, `P>|t|`, `Std.Err.`,tau) 
uqr_p4_results$var <- "p4"

uqr_p5_results <- urqCI(uqr_mod_p5, seed = 777, cluster = "province")
uqr_p5_results_names <- data.frame(name = rownames(uqr_p5_results$results))
uqr_p5_results <- cbind(uqr_p5_results_names, as.data.frame(uqr_p5_results$results))
rownames(uqr_p5_results) <- NULL
uqr_p5_results <- uqr_p5_results %>%
  select(name, Coef, `P>|t|`, `Std.Err.`,tau) 
uqr_p5_results$var <- "p5"

uqr_p6_results <- urqCI(uqr_mod_p6, seed = 777, cluster = "province")
uqr_p6_results_names <- data.frame(name = rownames(uqr_p6_results$results))
uqr_p6_results <- cbind(uqr_p6_results_names, as.data.frame(uqr_p6_results$results))
rownames(uqr_p6_results) <- NULL
uqr_p6_results <- uqr_p6_results %>%
  select(name, Coef, `P>|t|`, `Std.Err.`,tau) 
uqr_p6_results$var <- "p6"

all_results <- rbind(uqr_p1_results, uqr_p2_results, uqr_p3_results,
                     uqr_p4_results, uqr_p5_results, uqr_p6_results)

all_results <- all_results %>%
  filter(!grepl('_M', name))
         
write_xlsx(all_results, "output_data/all_deminsions_results.xlsx")
