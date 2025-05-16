library(haven)
library(dplyr)
library(lme4)
library(plm)
library(plotly)
library(patchwork)
library(ggplot2)
library(lmtest)
library(lfe)
library(clubSandwich)
library(biglmm)
library(quantreg)
library(data.table)
library(broom)
library(DescTools)
library(survival)
library(fixest)
library(tidyr)
library(AER)

rm(bartik_full_postdo)

data <- read_dta("D:/fullN_textiles0207.dta")

# Movers - lag j

movers = data %>%
  select(c("i", "year", "j"))

movers = movers %>%
  arrange(i, year)

setDT(movers)

movers = movers[!duplicated(movers), ]

movers[, lag_j := shift(j, type = "lag", n = 1)]


# Define job movers (ee)
movers <- movers %>%
  group_by(i) %>%
  mutate(
    ee = ifelse(year > min(year) & j != lag_j, 1, 0),
    stayer = ifelse(year > min(year) & j == lag_j, 1, 0)
  ) %>%
  ungroup()

data = data %>%
  left_join(movers, by = c("i", "year", "j"))

rm(movers)

gc()

# Unique firms per industry

df_industry_firms <- data %>%
  group_by(caem3, year) %>%  # Group by caem3 and year
  summarise(
    totaluniquefirms = n_distinct(j),  # Count unique firms
    .groups = "drop"
  )

data = data %>%
  left_join(df_industry_firms, by = c("caem3", "year"))

rm(df_industry_firms)

names(data)[names(data) == "totaluniquefirms.y"] <- "totaluniquefirms"

# firm labor productivity - number of workers

df <- data %>%
 mutate(
   vn = as.numeric(sales),           # Convert sales to numeric
   pemp = as.numeric(nworkers) # Convert num_workers to numeric
 ) %>%
  group_by(j, year) %>%
 summarise(
   fl_prod_workers = log(first(sales) / first(nworkers)),  # Compute sales per worker
   .groups = "drop"
 ) %>%
 ungroup()

data = data %>%
  left_join(df, by = c("j", "year"))

# firm labor productivity - hours of work

data = data %>%
  mutate(hours = hnormais + hextra)

data = data %>%
  group_by(j, year) %>%
  mutate(total_hours = sum(hours)) %>%
  ungroup()

df <- data %>%
  mutate(
    vn = as.numeric(sales),
    total_hours = as.numeric(total_hours)
  ) %>%
  group_by(j, year) %>%
  summarise(
    fl_prod_hours = log(first(sales) / first(total_hours)),  # Compute sales per worker
    .groups = "drop"
  ) %>%
  ungroup()

data = data %>%
  left_join(df, by = c("j", "year"))

# industry productivity - number of workers

df_industry <- data %>%
 group_by(caem3, year) %>%  # Group by industry and year
 summarise(
   total_sales = sum(sales, na.rm = TRUE),      # Sum of firm sales in the industry
   total_workers = sum(nworkers, na.rm = TRUE),  # Sum of workers in the industry
   .groups = "drop"
 ) %>%
 mutate(
   sl_prod_workers = ifelse(total_workers > 0, log(total_sales / total_workers), NA)  # Compute safely
 ) %>%
 arrange(caem3, year)  # Ensure proper ordering for lag calculation

data = data %>%
 left_join(df_industry, by = c("caem3", "year"))

data = data %>% select(-c("total_workers", "total_sales"))

# industry productivity - hours of work

df_industry <- data %>%
  group_by(caem3, year) %>%  # Group by industry and year
  summarise(
    total_sales = sum(sales, na.rm = TRUE),      # Sum of firm sales in the industry
    ind_total_hours = sum(total_hours, na.rm = TRUE),  # Sum of workers in the industry
    .groups = "drop"
  ) %>%
  mutate(
    sl_prod_hours = ifelse(ind_total_hours > 0, log(total_sales / ind_total_hours), NA)  # Compute safely
  ) %>%
  arrange(caem3, year)  # Ensure proper ordering for lag calculation

data = data %>%
  left_join(df_industry, by = c("caem3", "year"))

data = data %>% select(-c("ind_total_hours", "total_sales"))

# wage floors

mode_function <- function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

floors = data %>%
  group_by(caem3, prof_4d, year) %>%
  summarise(wage_floor = mode_function(rbase), .groups = "drop") %>%
  ungroup()

data = data %>%
  left_join(floors, by = c("caem3", "prof_4d", "year"))

################################ Descriptive Statistics #########################################

# industry time series plot

df_long <- data %>%
  pivot_longer(cols = c(sl_prod_hours, sl_prod_workers), names_to = "variable", values_to = "value")

df_summary <- df_long %>%
  group_by(year, variable) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            median_value = median(value, na.rm = TRUE),
            .groups = 'drop')


ts_industry = ggplot(df_summary, aes(x = year)) +
  geom_line(aes(y = mean_value, color = "Mean"), linewidth = 1) +
  geom_line(aes(y = median_value, color = "Median"), linewidth = 1, linetype = "dotted") +
  facet_wrap(~ variable, scales = "free_y") +
  geom_vline(xintercept = 2003, linetype = "dashed") +
  scale_color_manual(values = c("Mean" = "black", "Median" = "black")) +
  theme_minimal() +
  labs(title = "Time series of 3-digit industry labor productivity",
       y = "Value", color = "Statistic")

## wages and hours time series plot

df_long <- data %>%
  pivot_longer(cols = c(r_i_wages, hours), names_to = "variable", values_to = "value")

df_summary <- df_long %>%
  group_by(year, variable) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            median_value = median(value, na.rm = TRUE),
            .groups = 'drop')


ts_wages = ggplot(df_summary, aes(x = year)) +
  geom_line(aes(y = mean_value, color = "Mean"), linewidth = 1) +
  geom_line(aes(y = median_value, color = "Median"), linewidth = 1, linetype = "dotted") +
  facet_wrap(~ variable, scales = "free_y") +
  geom_vline(xintercept = 2003, linetype = "dashed") +
  scale_color_manual(values = c("Mean" = "black", "Median" = "black")) +
  theme_minimal() +
  labs(title = "Time series of 3-digit industry residual wages and worker hours",
       y = "Value", color = "Statistic")

ts = ts_industry / ts_wages
print(ts)

rm(df, df_industry, df_long, df_summary, ts_wages, ts_industry)

gc()

# treatment - quantifying different degrees of exposure pre-shock
## 1. pre-shock productivity gaps relative to national average - caem3
### Autor et al (2013), Pierce and Schott (2016), Bloom et al (2024)
### These studies motivate the use of pre-shock productivity gap as exposure proxy

sub_industry_prod <- data %>%
  group_by(caem3, year) %>%
  summarise(sub_industry_productivity_workers = mean(sl_prod_workers, na.rm = TRUE),
            sub_industry_productivity_hours = mean(sl_prod_hours, na.rm = TRUE))

national_prod <- data %>%
  group_by(year) %>%
  summarise(national_productivity_workers = mean(sl_prod_workers, na.rm = TRUE),
            national_productivity_hours = mean(sl_prod_hours, na.rm = TRUE))

prod_gap <- sub_industry_prod %>%
  left_join(national_prod, by = "year") %>%
  mutate(productivity_gap_workers = sub_industry_productivity_workers - national_productivity_workers,
         productivity_gap_hours = sub_industry_productivity_hours - national_productivity_hours)

pre_shock_gap <- prod_gap %>%
  filter(year == 2002) %>%
  group_by(caem3) %>%
  summarise(mean_gap_workers = mean(productivity_gap_workers, na.rm = TRUE),
            mean_gap_hours = mean(productivity_gap_hours, na.rm = TRUE))

pre_shock_gap <- pre_shock_gap %>%
  mutate(pos = mean_gap_hours >= 0)

gap1 = ggplot(pre_shock_gap, aes(x = reorder(caem3, mean_gap_hours), y = mean_gap_hours, fill = pos)) +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "#000", "FALSE" = "#CCC"), guide = "none") +
  coord_flip() +
  labs(x = "Sub-Industry", y = "Productivity Gap (Above/Below National Avg., 2002)",
       title = "Pre-Shock Productivity Gap by Sub-Industry") +
  theme_minimal()

rm(prod_gap, sub_industry_prod, national_prod) # below average: caem3 in {173, 177, 181, 182}

## 2. pre-shock labor intensity, indicated by avg worker wages - caem3
### Ebenstein et al. (2014), Dauth et al. (2017)
### These studies motivate the use of pre-shock labor intensity (low wages) as exposure proxy

sub_industry_prod <- data %>%
  group_by(caem3, year) %>%
  summarise(sub_industry_wages = mean(rganho, na.rm = TRUE))

national_prod <- data %>%
  group_by(year) %>%
  summarise(national_wages = mean(rganho, na.rm = TRUE))

prod_gap <- sub_industry_prod %>%
  left_join(national_prod, by = "year") %>%
  mutate(wage_gap = sub_industry_wages - national_wages)

pre_shock_gap <- prod_gap %>%
  filter(year == 2002) %>%
  group_by(caem3) %>%
  summarise(mean_gap = mean(wage_gap, na.rm = TRUE))

pre_shock_gap <- pre_shock_gap %>%
  mutate(pos = mean_gap >= 0)

gap2 = ggplot(pre_shock_gap, aes(x = reorder(caem3, mean_gap), y = mean_gap, fill = pos)) +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "#000", "FALSE" = "#CCC"), guide = "none") +
  coord_flip() +
  labs(x = "Sub-Industry", y = "Wage Gap (Above/Below National Avg., 2002)",
       title = "Pre-Shock Wage Gap by Sub-Industry") +
  theme_minimal()

rm(prod_gap, sub_industry_prod, national_prod) # below average: caem3 in {181, 182}

gap = gap1 + gap2
print(gap)

# defining shock post*exposure - sub-industries 181 and 182

data = data %>%
  mutate(shock = ifelse(year>=2003 & caem3 %in% {181; 182}, 1, 0),
         treated = ifelse(caem3 %in% {181; 182}, 1, 0))

feols = feols(r_i_wages ~ shock | year, data = data)
summary(feols)

data$predicted_wage = predict(feols, newdata = data)

plot_data <- data %>%
  group_by(year, treated) %>%
  summarise(mean_wage = mean(predicted_wage, na.rm = TRUE))

## treated observations missing in 2007, following restructuring and consolidation
## clothing manufacturers integrated upstream (into textiles), and textile firms moved downstream
## truett and truett, 2019; amador and opromolla, 2009


ggplot(plot_data, aes(x = year, y = mean_wage, color = factor(treated))) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 2003, linetype = "dashed", color = "black", linewidth = 0.8) +
  scale_color_manual(
    values = c("#0072B2", "#D55E00"),  # Custom colors
    labels = c("Control", "Treated")
  ) +
  labs(
    title = "Predicted Residual Wages by Treatment Group",
    x = "Year",
    y = "Predicted Residual Wage",
    color = "Group"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

## increased global competition, the end of protective trade agreements, and the need for modernization 
## led to the absorption of smaller firms by larger ones, vertical integration, and a shift towards higher value-added activities

## do industries w/ higher floor bite exhibit smaller adjustments to the shock?

bite = floors %>% filter(year==2003)
bite = bite %>% filter(wage_floor!=0)
bite = bite %>% select(-year)
names(bite)[names(bite) == "wage_floor"] = "wage_floor_2003"

data = data %>%
  left_join(bite, by = c("caem3", "prof_4d"))

rm(bite)

df02 = data %>%
  filter(year==2002) %>%
  mutate(floor_bite03 = rbase - wage_floor_2003)

df02 <- df02 %>%
  mutate(gap_decile = ntile(floor_bite03, 10))

data <- data %>%
  left_join(df02 %>% select(i, gap_decile), by = "i")

did_bite <- feols(
  r_i_wages ~ shock:factor(gap_decile) | year + j + gap_decile,
  data = data,
  cluster = ~caem3
) 
# adding gap_decile FEs ensures within-decile comparability. Important in heterogeneous treatment effects, as
# prevents DiD estimate from being driven by differences, e.g., between lowest and highest decile groups. Moreover,
# inherent unobserved heterogeneity across deciles - motivation of whole inequality debate

summary(did_bite)

library(readr)

coef_df <- broom::tidy(did_bite, conf.int = TRUE) %>%
  filter(grepl("shock:factor", term)) %>%
  mutate(decile = parse_number(term))

ggplot(coef_df, aes(x = decile, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Wage Gap Decile (1=Smallest Gap)", 
       y = "Treatment Effect (DiD Coefficient)",
       title = "Heterogeneous Treatment Effects by Pre-Shock Wage Gap") +
  theme_minimal()


decile_values <- df02 %>%  
  group_by(gap_decile) %>%
  summarise(median_gap = median(floor_bite03, na.rm = TRUE))

coef_df <- coef_df %>%
  left_join(decile_values, by = c("decile" = "gap_decile")) %>%
  mutate(
    decile_label = sprintf("%d (%.2f)", decile, median_gap)  # Format: "1 (0.05)"
  )

ggplot(coef_df, aes(x = decile, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(
    breaks = 1:10,
    labels = coef_df$decile_label
  ) +
  labs(
    x = "Wage Gap Decile (1=First Decile)", 
    y = "Treatment Effect (DiD Coefficient)",
    title = "Heterogeneous Treatment Effects by Pre-Shock Wage Gap"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

rm(coef_df, decile_values, df02, plot_data, pre_shock_gap)

gc()

print(colnames(data))

# comparing wage/employment trajectories of firms near floor pre-shock vs those far away

plot_data <- data %>%
  filter(year %in% 2002:2007) %>%
  group_by(year, gap_decile) %>%
  summarise(
    mean_wage = mean(r_i_wages, na.rm = TRUE),
    mean_employment = mean(nworkers, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_longer(
    cols = c(mean_wage, mean_employment),
    names_to = "metric",
    values_to = "value"
  )

ggplot(plot_data, aes(x = year, y = value, group = gap_decile, color = gap_decile)) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = 2003, linetype = "dashed", color = "red", linewidth = 0.8) +
  scale_color_viridis_c(
    name = "Wage Gap Decile",
    option = "viridis",
    breaks = 1:10,
    guide = guide_colorbar(barwidth = 12, title.position = "top")
  ) +
  facet_wrap(~ metric, scales = "free_y", labeller = labeller(
    metric = c(mean_wage = "Average Wage", mean_employment = "Average Employment")
  )) +
  labs(
    x = "Year",
    y = "Value",
    title = "Wage and Employment Trajectories by Pre-Shock Wage Gap Decile",
    subtitle = "Vertical line indicates 2003 China WTO shock"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  ) +
  scale_x_continuous(breaks = seq(2002, 2007, 1))
















