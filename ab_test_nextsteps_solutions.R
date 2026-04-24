#' ab_test_nextsteps.R
#'
#' Code accompanying Lecture: A/B Tests: Next Steps
#' of Social Media and Web Analytics
#' at TiSEM in 2026
#'
#'

# --- Libraries --- #
library(readr)
library(dplyr)
library(infer)
library(ggplot2)
library(broom)
library(estimatr)

# --- Part 1: Partial Compliance: ITT and TOT --- #

# Load the ad experiment data
ad_data <- read_csv("data/adfx2025.csv")
head(ad_data)

# ITT Estimation
model_itt <- lm(sales ~ Treatment, data = ad_data)
tidy(model_itt) %>%
  mutate(lower_ci = estimate - 1.96 * std.error,
         upper_ci = estimate + 1.96 * std.error)

# TOT Estimation
ad_data_exposed <- ad_data %>% filter(saw_ads == TRUE)

model_tot <- lm(sales ~ Treatment, data = ad_data_exposed)
tidy(model_tot) %>%
  mutate(lower_ci = estimate - 1.96 * std.error,
         upper_ci = estimate + 1.96 * std.error)

# Comparing ITT and TOT
itt_res <- tidy(model_itt) %>% filter(term == "Treatment") %>%
  mutate(estimator = "ITT")
tot_res <- tidy(model_tot) %>% filter(term == "Treatment") %>%
  mutate(estimator = "TOT")
both <- bind_rows(itt_res, tot_res) %>%
  mutate(lower_ci = estimate - 1.96 * std.error,
         upper_ci = estimate + 1.96 * std.error)

ggplot(both, aes(x = estimator, y = estimate)) +
  geom_pointrange(aes(ymin = lower_ci, ymax = upper_ci)) +
  geom_hline(yintercept = 0, color = "red") +
  ylab("Point Estimate") + xlab("")

# --- Part 2: Variance Reduction with CUPED --- #

# Load the data
df <- read_csv("data/ad_campaign.csv")
head(df)

# Standard ATE estimation
mod <- lm(post_spend ~ treatment_status,
          data = df)
tidy(mod)

# CUPED: Estimating theta
theta <-
    tidy(lm(post_spend ~ pre_spend, data = df)) %>%
    filter(term=="pre_spend") %>%
    select(estimate) %>%
    purrr::pluck('estimate')

print(theta)

#alternative:
#cov(df$post_spend, df$pre_spend) / var(df$pre_spend)

# CUPED: Computing Y_CUPED
df <-
    df %>%
    mutate(cuped_spend = post_spend -
               theta*(pre_spend)
           )

# CUPED: Estimate the ATE
mod_cuped <- lm(cuped_spend ~ treatment_status,
                data = df)
tidy(mod_cuped)

# --- Part 3: Clustered Standard Errors --- #

# Load the data
recommender <- read_csv("data/recommender_clustering.csv")

head(recommender)

# Estimate the ATE the 'usual' way
tidy(lm(log(revenue) ~ treatment_status, data = recommender))

# Heteroskedasticity Robust Standard Errors
tidy(lm_robust(log(revenue) ~ treatment_status,
               data = recommender,
               se_type = "HC1"), conf.int = FALSE
     )

# Cluster Robust Standard Errors
tidy(lm_robust(log(revenue) ~ treatment_status,
               data = recommender,
               cluster = user), conf.int = FALSE
     )
