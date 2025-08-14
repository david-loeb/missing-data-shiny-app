pacman::p_load(dplyr, tibble, stringr, tidyr, purrr)
setwd(this.path::here())
set.seed(8)

# ==== Create data =============================================================

df <- tibble(
  id = 1:660,
  x = rnorm(660, 80, 19),
  ypre = .08 * x + rnorm(660, 3.8, 2),
  treat = c(rep(0, 336), rep(1, 324)),
  y = ypre + treat * 3 + rnorm(660, 3, 1),
  # Create missing data indicators under diff mechanisms
  miss.mcar = sample(c(0,1), 660, replace = T, prob = c(.75, .25)),
  miss.x = ifelse(percent_rank(x) <= .25, 1, 0),
  miss.x_rand = ifelse(percent_rank(x + rnorm(660, sd = 5)) <= .25, 1, 0),
  miss.x_trt = ifelse(percent_rank(x + 20 * treat) <= .25, 1, 0),
  miss.ypre = ifelse(percent_rank(ypre) >= .75, 1, 0),
  miss.ypre_trt = ifelse(percent_rank(ypre + 3 * treat) >= .75, 1, 0),
  miss.y = ifelse(percent_rank(y) <= .25, 1, 0),
  miss.y_rand = ifelse(percent_rank(y + rnorm(660, sd = 1)) <= .25, 1, 0),
  miss.y_trt = ifelse(percent_rank(y + 3 * treat) <= .25, 1, 0),
) |> 
  pivot_longer(
    starts_with('miss'),
    names_to = c('.values', 'miss_mech'),
    names_sep = '\\.'
  ) |> 
  select(-`.values`) |> 
  rename(miss = value)

df <- bind_rows(
  mutate(df, miss_handle = 'cmplt', miss_var = 'y', pred = 'x'),
  mutate(df, miss_handle = 'cmplt', miss_var = 'x', pred = 'x'),
  mutate(df, miss_handle = 'cmplt', miss_var = 'y', pred = 'ypre'),
  mutate(df, miss_handle = 'cmplt', miss_var = 'ypre', pred = 'ypre'),
  mutate(df, miss_handle = 'lwd', miss_var = 'y', pred = 'x'),
  mutate(df, miss_handle = 'lwd', miss_var = 'x', pred = 'x'),
  mutate(df, miss_handle = 'lwd', miss_var = 'y', pred = 'ypre'),
  mutate(df, miss_handle = 'lwd', miss_var = 'ypre', pred = 'ypre'),
  mutate(df, miss_handle = 'mean_imp', miss_var = 'y', pred = 'x'),
  mutate(df, miss_handle = 'mean_imp', miss_var = 'x', pred = 'x'),
  mutate(df, miss_handle = 'mean_imp', miss_var = 'y', pred = 'ypre'),
  mutate(df, miss_handle = 'mean_imp', miss_var = 'ypre', pred = 'ypre'),
  mutate(df, miss_handle = 'fiml', miss_var = 'y', pred = 'x'),
  mutate(df, miss_handle = 'fiml', miss_var = 'x', pred = 'x'),
  mutate(df, miss_handle = 'fiml', miss_var = 'y', pred = 'ypre'),
  mutate(df, miss_handle = 'fiml', miss_var = 'ypre', pred = 'ypre'),
  mutate(df, miss_handle = 'mult_imp', miss_var = 'y', pred = 'x'),
  mutate(df, miss_handle = 'mult_imp', miss_var = 'x', pred = 'x'),
  mutate(df, miss_handle = 'mult_imp', miss_var = 'y', pred = 'ypre'),
  mutate(df, miss_handle = 'mult_imp', miss_var = 'ypre', pred = 'ypre')
) |> 
  mutate(
    y = case_when(
      miss == 1 & miss_var == 'y' & miss_handle == 'mean_imp' ~ mean(y[miss == 0]),
      miss == 1 & miss_var == 'y' & miss_handle != 'cmplt' ~ NA,
      .default = y
    ),
    x = case_when(
      miss == 1 & miss_var == 'x' & miss_handle == 'mean_imp' ~ mean(x[miss == 0]),
      miss == 1 & miss_var == 'x' & miss_handle != 'cmplt' ~ NA,
      .default = x
    ),
    ypre = case_when(
      miss == 1 & miss_var == 'ypre' & miss_handle == 'mean_imp' ~ mean(ypre[miss == 0]),
      miss == 1 & miss_var == 'ypre' & miss_handle != 'cmplt' ~ NA,
      .default = ypre
    ),
    y_plt = case_when(
      is.na(y) & miss_handle == 'lwd' ~ NA,
      is.na(y) & miss_handle == 'fiml' ~ 0,
      .default = y
    ),
    x_plt = case_when(
      is.na(x) & miss_handle == 'lwd' ~ NA,
      is.na(x) & miss_handle == 'fiml' ~ 10,
      .default = x
    ),
    ypre_plt = case_when(
      is.na(ypre) & miss_handle == 'lwd' ~ NA,
      is.na(ypre) & miss_handle == 'fiml' ~ 0,
      .default = ypre
    ),
    full_spec = str_c(miss_handle, '.', miss_mech, '.', miss_var, '.', pred)
  )

# ==== Generate params for adding correct reg lines to plots ===================

params <- expand_grid(
  miss_handle = unique(df$miss_handle[df$miss_handle != 'cmplt']),
  miss_mech = unique(df$miss_mech),
  miss_var = c('y', 'x', 'ypre'),
  pred = c('x', 'ypre')
) |> 
  filter(!(miss_var == 'x' & pred == 'ypre' | miss_var == 'ypre' & pred == 'x'))

## --- Full Data ---------------------------------------------------------------

mod_x <- lm(
  y ~ treat + x, filter(df, miss_handle == 'cmplt') |> distinct(id, .keep_all = T)
)
mod_ypre <- lm(
  y ~ treat + ypre, filter(df, miss_handle == 'cmplt') |> distinct(id, .keep_all = T)
)
res_full_x <- tibble(
  int = c(coef(mod_x)[[1]]),
  coef_treat = c(coef(mod_x)[[2]]),
  se_treat = c(sqrt(diag(vcov(mod_x)))[[2]]),
  coef_x = c(coef(mod_x)[[3]]),
  se_x = c(sqrt(diag(vcov(mod_x)))[[3]])
) |> 
  expand_grid(
    miss_mech = unique(df$miss_mech), miss_var = c('y', 'x'), pred = 'x'
  )
res_full_ypre <- tibble(
  int = c(coef(mod_ypre)[[1]]),
  coef_treat = c(coef(mod_ypre)[[2]]),
  se_treat = c(sqrt(diag(vcov(mod_ypre)))[[2]]),
  coef_x = c(coef(mod_ypre)[[3]]),
  se_x = c(sqrt(diag(vcov(mod_ypre)))[[3]])
) |> 
  expand_grid(
    miss_mech = unique(df$miss_mech), miss_var = c('y', 'ypre'), pred = 'ypre'
  )
res_full <- bind_rows(res_full_x, res_full_ypre) |> 
  mutate(full_spec = str_c('cmplt.', miss_mech, '.', miss_var, '.', pred)) |> 
  select(full_spec, int, coef_treat, se_treat, coef_x, se_x)
rm(mod_x, mod_ypre, res_full_x, res_full_ypre)

## --- LWD, Mean Imp & FIML ----------------------------------------------------

run_mods <- function(mis_handle, mis_mech, mis_var, prd) {
  if (mis_handle == 'lwd') {
    lm(
      as.formula(str_c('y ~ treat +', prd)), 
      df |> filter(
        miss_handle == 'lwd', miss_mech == mis_mech, 
        miss_var == mis_var, pred == prd
      ),
      na.action = na.omit
    )
  } else if (mis_handle == 'mean_imp') {
    lm(
      as.formula(str_c('y ~ treat +', prd, '+ miss')), 
      df |> filter(
        miss_handle == 'mean_imp', miss_mech == mis_mech, 
        miss_var == mis_var, pred == prd
      )
    )
  } else if (mis_handle == 'fiml') {
    lavaan::sem(
      str_c('y ~ treat +', prd),
      df |> filter(
        miss_handle == 'fiml', miss_mech == mis_mech, 
        miss_var == mis_var, pred == prd
      ),
      missing = 'fiml.x'
    )
  }
}

res <- pmap(params, ~ run_mods(..1, ..2, ..3, ..4)) |> 
  set_names(
    str_c(
      params$miss_handle, '.', params$miss_mech, '.', 
      params$miss_var, '.', params$pred
    )
  ) |>
  map(broom::tidy) |> 
  list_rbind(names_to = 'full_spec') |> 
  mutate(
    term = case_match(
      term,
      c('(Intercept)', 'y ~1 ') ~ 'int',
      'y ~ treat' ~ 'treat',
      c('y ~ x', 'y ~ ypre', 'ypre') ~ 'x',
      .default = term
    )
  ) |> 
  filter(term %in% c('int', 'treat', 'x')) |> 
  select(full_spec, term, coef = estimate, se = std.error) |> 
  pivot_wider(names_from = term, values_from = c(coef, se)) |> 
  select(full_spec, int = coef_int, coef_treat, se_treat, coef_x, se_x)

## --- Mult Imp ----------------------------------------------------------------

impute_data <- function(mis_mech, mis_var, prd) {
  if (grepl('x|ypre', mis_var)) {
    fxd_vars <- 'treat'
  } else if (prd == 'x') {
    fxd_vars <- 'x treat'
  } else {
    fxd_vars <- 'ypre treat'
  }
  df <- df |> 
    filter(
      miss_handle == 'mult_imp', miss_mech == mis_mech, 
      miss_var == mis_var, pred == prd
    ) |> 
    select(id, x, ypre, treat, y)
  mod <- rblimp::rblimp(
    data = df,
    ordinal = 'treat',
    fixed = fxd_vars,
    model = str_c('y ~ treat ', prd, ';'),
    seed = 6,
    burn = 5000,
    iter = 5000,
    nimps = 20,
    chains = 10,
    options = 'labels'
  )
  map(1:20, ~ mod@imputations[[.x]] |> mutate(imp_num = .x)) |> 
    list_rbind()
}

df_imp <- pmap(
  filter(params, miss_handle == 'mult_imp'), ~ impute_data(..2, ..3, ..4)
) |>
  set_names(str_c(
    'mult_imp.',
    params$miss_mech[params$miss_handle == 'mult_imp'], '.',
    params$miss_var[params$miss_handle == 'mult_imp'], '.',
    params$pred[params$miss_handle == 'mult_imp']
  )) |>
  list_rbind(names_to = 'full_spec')

run_mod_mult_imp <- function(ful_spec) {
  df_imp <- filter(df_imp, full_spec == ful_spec)
  if (grepl('ypre$', ful_spec)) prd <- 'ypre' else prd <- 'x'
  mod_res <- list()
  for (i in 1:20) {
    mod <- lm(
      as.formula(str_c('y ~ treat +', prd)),
      filter(df_imp, imp_num == i)
    )
    mod_res[[i]] <- broom::tidy(mod)
  }
  list_rbind(mod_res) |> 
    rename(coef = estimate, se = std.error) |> 
    mutate(var = se^2) |> 
    summarise(
      .by = term,
      coef_final = mean(coef),
      Uhat = mean(var),  # within imputation variance
      B = (sum((coef - mean(coef))^2)) / (20-1),  # btwn imp variance
      Tv = Uhat + (1 + 1/20) * B,  # total variance
      se_final = sqrt(Tv),  # standard error for parameter
    ) |> 
    mutate(
      term = case_match(
        term, '(Intercept)' ~ 'int', 'ypre' ~ 'x', .default = term
      ),
      full_spec = ful_spec
    ) |> 
    select(full_spec, term, coef = coef_final, se = se_final) |> 
    pivot_wider(names_from = term, values_from = c(coef, se)) |> 
    select(full_spec, int = coef_int, coef_treat, se_treat, coef_x, se_x)
}

res_mult_imp <- map(unique(df_imp$full_spec), run_mod_mult_imp) |> list_rbind()

## --- Merge onto main df ------------------------------------------------------

res <- bind_rows(res_full, res, res_mult_imp)
df <- left_join(df, res, 'full_spec')

# ==== Add multiple imputation data for plotting ===============================

df <- df_imp |> 
  summarise(
    x_mean = mean(x),
    x_B = (sum((x - mean(x))^2)) / (20-1),
    x_final = x_mean + rnorm(1, sd = sqrt(x_B)),
    ypre_mean = mean(ypre),
    ypre_B = (sum((ypre - mean(ypre))^2)) / (20-1),
    ypre_final = ypre_mean + rnorm(1, sd = sqrt(ypre_B)),
    y_mean = mean(y),
    y_B = (sum((y - mean(y))^2)) / (20-1),
    y_final = y_mean + rnorm(1, sd = sqrt(y_B)),
    .by = c(id, full_spec)
  ) %>%
  left_join(df, ., c('id', 'full_spec')) |> 
  mutate(
    x_plt = ifelse(!is.na(x_final) & pred == 'x', x_final, x_plt),
    ypre_plt = ifelse(!is.na(ypre_final) & pred == 'ypre', ypre_final, ypre_plt),
    y_plt = ifelse(!is.na(y_final), y_final, y_plt)
  ) |> 
  select(-x_final, -ypre_final, -y_final)

rm(  # cleanup
  df_imp, params, res_full, res_mult_imp, impute_data, run_mod_mult_imp, run_mods
)

# ==== Save for Shiny ==========================================================

arrow::write_parquet(df, 'data/data.parquet')
arrow::write_parquet(res, 'data/results.parquet')
