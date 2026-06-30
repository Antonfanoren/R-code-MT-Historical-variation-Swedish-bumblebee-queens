# R-code-MT-Historical-variation-Swedish-bumblebee-queens
R code for master's thesis "Historical variation in queen body size of Swedish bumblebees"


# Bombus queen ITD analysis

This folder is an R script for reproducing the thesis analyses.

## File

- `bombus_thesis_analysis_ordered.R`

## How to run

1. Put your raw ITD Excel file and spring climate CSV file in the same folder as the script.
2. Open the script in RStudio.
3. Edit these two lines near the top:

```r
itd_file <- "Bombus_ITD_AN.xlsx"
climate_file <- "climate_spring_regions.csv"
```

4. Run the script from top to bottom.
5. Outputs are written to an `outputs/` folder.




--------------------------------------------
# Bombus queen ITD analysis
# Ordered R script for reproducible thesis analyses
--------------------------------------------
#
# HOW TO USE
# 1. Put your raw ITD Excel file and climate CSV file in the same folder.
# 2. Edit the three paths below.
# 3. Run the script from top to bottom.
#
# Required input files:
# - Bombus_ITD_AN.xlsx or equivalent raw ITD file
# - Regional spring climate CSV with columns:
#   region, year, temp_mean_spring, pr_sum_spring
#
# Main outputs:
# - Tables as CSV files in output_dir
# - Figures as PDF files in output_dir
--------------------------------------------


--------------------------------------------
# 0. Packages
--------------------------------------------
packages <- c(
  "readxl", "ggplot2", "dplyr", "tidyr", "patchwork",
  "broom", "emmeans", "car", "lme4", "lmerTest",
  "ggeffects", "broom.mixed", "maps"
)

installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(broom)
library(emmeans)
library(car)
library(lme4)
library(lmerTest)
library(ggeffects)
library(broom.mixed)
library(maps)


--------------------------------------------
# 1. File paths
--------------------------------------------

# EDIT THESE PATHS
itd_file <- "Bombus_ITD_AN.xlsx"
climate_file <- "climate_spring_regions.csv"

# Output folder
output_dir <- "outputs"
dir.create(output_dir, showWarnings = FALSE)


--------------------------------------------
# 2. Import ITD data
--------------------------------------------
Bombus_ITD_AN <- read_excel(itd_file)

bee_new <- Bombus_ITD_AN
names(bee_new) <- make.names(names(bee_new))


--------------------------------------------
# 3. Clean species names and core variables
--------------------------------------------
bee_new <- bee_new %>%
  mutate(
    Species_raw = as.character(Species),
    Species_clean = tolower(Species_raw),

    Species = case_when(
      grepl("lapidarius", Species_clean) ~ "Bombus lapidarius",
      grepl("pascuorum|pascorum|pascoroum", Species_clean) ~ "Bombus pascuorum",
      grepl("sylvarum", Species_clean) ~ "Bombus sylvarum",
      grepl("terrestris|terestris", Species_clean) ~ "Bombus terrestris",
      TRUE ~ NA_character_
    ),

    county = as.character(county),
    Locality = as.character(Locality),
    year = as.numeric(year),
    decade = as.factor(decade),
    ITD1 = as.numeric(ITD1),
    ITD2 = as.numeric(ITD2),
    ITD3 = as.numeric(ITD3),
    ITDaverage = as.numeric(ITDaverage),
    decimalLatitude = as.numeric(decimalLatitude),
    decimalLongitude = as.numeric(decimalLongitude),
    julian.day = as.numeric(julian.day)
  ) %>%
  filter(!is.na(Species))


--------------------------------------------
# 4. Assign focal regions
--------------------------------------------

bee_new <- bee_new %>%
  mutate(
    county_clean = tolower(county),

    region_code = case_when(
      grepl("skåne|skane|^sk$", county_clean) ~ "SK",
      grepl("västra|vastra|västergötland|vastergotland|västra götaland|vastra gotaland|^vg$", county_clean) ~ "VG",
      grepl("östergötland|ostergotland|östra götaland|ostra gotaland|^ög$|^og$", county_clean) ~ "OG",
      TRUE ~ NA_character_
    ),

    region_climate = case_when(
      region_code == "SK" ~ "Skane",
      region_code == "VG" ~ "Vastergotland",
      region_code == "OG" ~ "Ostergotland",
      TRUE ~ NA_character_
    ),

    region_label = case_when(
      region_code == "SK" ~ "Skåne",
      region_code == "VG" ~ "Västra Götaland",
      region_code == "OG" ~ "Östergötland",
      TRUE ~ NA_character_
    )
  )


--------------------------------------------
# 5. Retain queen-sized specimens using species-specific ITD thresholds
--------------------------------------------

bee_new <- bee_new %>%
  mutate(
    queen_threshold = case_when(
      Species == "Bombus lapidarius" ~ 5.376667,
      Species == "Bombus pascuorum" ~ 4.396667,
      Species == "Bombus sylvarum" ~ 4.643333,
      Species == "Bombus terrestris" ~ 5.25,
      TRUE ~ NA_real_
    ),

    queen_check = case_when(
      ITDaverage >= queen_threshold ~ "above_threshold",
      ITDaverage < queen_threshold ~ "below_threshold",
      TRUE ~ NA_character_
    )
  )


--------------------------------------------
# 6. Filter final pre-1960 bee dataset
--------------------------------------------

bee_old <- bee_new %>%
  filter(
    year <= 1960,
    queen_check == "above_threshold",
    !is.na(year),
    !is.na(ITDaverage),
    !is.na(region_climate)
  )


--------------------------------------------
# 7. Import and clean climate data
--------------------------------------------

climate <- read.csv2(climate_file)

climate <- climate %>%
  mutate(
    region = as.character(region),
    year = as.numeric(year),
    temp_mean_spring = as.numeric(temp_mean_spring),
    pr_sum_spring = as.numeric(pr_sum_spring),

    region_label = case_when(
      region == "Skane" ~ "Skåne",
      region == "Vastergotland" ~ "Västra Götaland",
      region == "Ostergotland" ~ "Östergötland",
      TRUE ~ region
    )
  )

climate_old <- climate %>%
  filter(year <= 1960)


--------------------------------------------
# 8. Merge bee and climate data
--------------------------------------------

bee_clim <- bee_old %>%
  left_join(
    climate,
    by = c("region_climate" = "region", "year" = "year")
  ) %>%
  mutate(
    region_label = region_label.x
  )

bee_clim$Species <- factor(
  bee_clim$Species,
  levels = c(
    "Bombus lapidarius",
    "Bombus pascuorum",
    "Bombus sylvarum",
    "Bombus terrestris"
  )
)

bee_clim$region_label <- factor(
  bee_clim$region_label,
  levels = c("Skåne", "Västra Götaland", "Östergötland")
)

climate_old$region_label <- factor(
  climate_old$region_label,
  levels = c("Skåne", "Västra Götaland", "Östergötland")
)


--------------------------------------------
# 9. Plot settings
--------------------------------------------

species_cols <- c(
  "Bombus lapidarius" = "#ca0020",
  "Bombus pascuorum" = "#f4a582",
  "Bombus sylvarum" = "#009E73",
  "Bombus terrestris" = "#0581b0"
)

region_cols <- c(
  "Skåne" = "#e9a3c9",
  "Västra Götaland" = "#1b7837",
  "Östergötland" = "#542788"
)

species_labs <- function(x) {
  parse(text = paste0("italic('", x, "')"))
}

species_facet_labs <- as_labeller(
  function(x) paste0("italic('", x, "')"),
  default = label_parsed
)

theme_bee <- function() {
  theme_classic(base_size = 13) +
    theme(
      axis.title = element_text(size = 13),
      axis.text = element_text(size = 11, color = "black"),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 11),
      strip.background = element_rect(fill = "grey90", color = "black"),
      strip.text = element_text(size = 12, face = "italic"),
      legend.position = "right"
    )
}


--------------------------------------------
# 10. Table 1: Sample size by species and region
--------------------------------------------
table1_sample_size <- bee_clim %>%
  group_by(Species, region_label) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = region_label,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(Total = rowSums(across(where(is.numeric)))) %>%
  bind_rows(
    summarise(
      .,
      Species = "Total",
      Skåne = sum(Skåne),
      `Västra Götaland` = sum(`Västra Götaland`),
      Östergötland = sum(Östergötland),
      Total = sum(Total)
    )
  )

write.csv2(
  table1_sample_size,
  file.path(output_dir, "Table_1_sample_size_species_region.csv"),
  row.names = FALSE
)

--------------------------------------------
# 11.Geographic sampling distribution
--------------------------------------------

sweden_map <- map_data("world") %>%
  filter(region %in% c("Sweden", "Norway", "Finland", "Denmark"))

p_map <- ggplot() +
  geom_polygon(
    data = sweden_map,
    aes(x = long, y = lat, group = group),
    fill = "grey90",
    color = "grey40",
    linewidth = 0.3
  ) +
  geom_point(
    data = bee_clim,
    aes(x = decimalLongitude, y = decimalLatitude, color = Species),
    alpha = 0.75,
    size = 1.8
  ) +
  scale_color_manual(values = species_cols) +
  coord_quickmap(
    xlim = c(10, 25),
    ylim = c(54.5, 69.5)
  ) +
  labs(
    x = "Longitude",
    y = "Latitude",
    color = "Species"
  ) +
  theme_bee()

ggsave(
  file.path(output_dir, "Figure_sampling_map.pdf"),
  p_map,
  width = 7,
  height = 8
)


--------------------------------------------
# 12. Figure 1: ITD distribution by species
--------------------------------------------
p_fig1_species <- ggplot(
  bee_clim,
  aes(x = Species, y = ITDaverage, fill = Species)
) +
  geom_boxplot(alpha = 0.85, outlier.shape = NA, width = 0.65) +
  geom_jitter(
    aes(color = Species),
    width = 0.15,
    alpha = 0.45,
    size = 1.6
  ) +
  scale_fill_manual(values = species_cols) +
  scale_color_manual(values = species_cols) +
  scale_x_discrete(labels = species_labs) +
  labs(
    x = NULL,
    y = "Inter-tegular distance (mm)"
  ) +
  theme_bee() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 25, hjust = 1)
  )

ggsave(
  file.path(output_dir, "Figure_1_ITD_distribution_species.pdf"),
  p_fig1_species,
  width = 7,
  height = 5
)


--------------------------------------------
# 13. Table 2: Global species model and Tukey pairwise comparisons
--------------------------------------------

bee_clim$Species <- relevel(
  factor(bee_clim$Species),
  ref = "Bombus lapidarius"
)

m_species <- lm(ITDaverage ~ Species, data = bee_clim)

table2_species_anova <- Anova(m_species, type = 2) %>%
  as.data.frame()

emm_species <- emmeans(m_species, ~ Species)

table2_pairwise_species <- pairs(
  emm_species,
  adjust = "tukey"
) %>%
  as.data.frame() %>%
  transmute(
    Comparison = contrast,
    Difference = round(estimate, 3),
    SE = round(SE, 3),
    df = round(df, 0),
    t = round(t.ratio, 2),
    p = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
  )

write.csv2(
  table2_species_anova,
  file.path(output_dir, "Table_2a_global_species_ANOVA.csv"),
  row.names = TRUE
)

write.csv2(
  table2_pairwise_species,
  file.path(output_dir, "Table_2_pairwise_species_Tukey.csv"),
  row.names = FALSE
)


--------------------------------------------
# 14. Prepare dataset for hierarchical models
--------------------------------------------
bee_mod <- bee_clim %>%
  filter(
    !is.na(ITDaverage),
    !is.na(year),
    !is.na(temp_mean_spring),
    !is.na(pr_sum_spring),
    !is.na(Species),
    !is.na(region_label)
  ) %>%
  mutate(
    Species = factor(Species),
    region_label = factor(region_label),
    year_f = factor(year),
    year_c = as.numeric(scale(year, center = TRUE, scale = FALSE)),
    temp_c = as.numeric(scale(temp_mean_spring, center = TRUE, scale = FALSE)),
    pr_c = as.numeric(scale(pr_sum_spring, center = TRUE, scale = FALSE))
  )

mean_year <- mean(bee_mod$year, na.rm = TRUE)
mean_temp <- mean(bee_mod$temp_mean_spring, na.rm = TRUE)
mean_pr <- mean(bee_mod$pr_sum_spring, na.rm = TRUE)


--------------------------------------------
# 15. Table 3 and Figure 2: ITD versus collection year
--------------------------------------------

m_year_h <- lmer(
  ITDaverage ~ year_c * Species + (1 | year_f) + (1 | region_label),
  data = bee_mod,
  REML = FALSE,
  control = lmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 100000)
  )
)

pred_year_h <- ggpredict(
  m_year_h,
  terms = c("year_c [all]", "Species")
) %>%
  as.data.frame() %>%
  mutate(
    year = x + mean_year,
    Species = group
  )

p_fig2_year <- ggplot() +
  geom_point(
    data = bee_mod,
    aes(x = year, y = ITDaverage, color = Species),
    alpha = 0.55,
    size = 1.7
  ) +
  geom_ribbon(
    data = pred_year_h,
    aes(x = year, ymin = conf.low, ymax = conf.high, fill = Species),
    alpha = 0.18,
    inherit.aes = FALSE
  ) +
  geom_line(
    data = pred_year_h,
    aes(x = year, y = predicted, color = Species),
    linewidth = 1
  ) +
  facet_wrap(~ Species, scales = "free_y", labeller = species_facet_labs) +
  scale_color_manual(values = species_cols) +
  scale_fill_manual(values = species_cols) +
  labs(
    x = "Year",
    y = "Inter-tegular distance (mm)",
    color = "Species",
    fill = "Species"
  ) +
  theme_bee() +
  theme(legend.position = "none")

ggsave(
  file.path(output_dir, "Figure_2_ITD_year_hierarchical.pdf"),
  p_fig2_year,
  width = 8,
  height = 6
)


--------------------------------------------
# 16. Figure 3 and Table 4a-b: Spring climate trends
--------------------------------------------

p_temp_time <- ggplot(
  climate_old,
  aes(x = year, y = temp_mean_spring, color = region_label)
) +
  geom_point(alpha = 0.45, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.9) +
  scale_color_manual(values = region_cols) +
  labs(
    x = "Year",
    y = "Mean spring temperature (°C)",
    color = "Region"
  ) +
  theme_bee()

p_pr_time <- ggplot(
  climate_old,
  aes(x = year, y = pr_sum_spring, color = region_label)
) +
  geom_point(alpha = 0.45, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.9) +
  scale_color_manual(values = region_cols) +
  labs(
    x = "Year",
    y = "Spring precipitation sum (mm)",
    color = "Region"
  ) +
  theme_bee()

p_fig3_climate <- p_temp_time / p_pr_time

ggsave(
  file.path(output_dir, "Figure_3_spring_climate_trends.pdf"),
  p_fig3_climate,
  width = 8,
  height = 8
)

m_temp_time_overall <- lm(temp_mean_spring ~ year, data = climate_old)
m_pr_time_overall <- lm(pr_sum_spring ~ year, data = climate_old)

table4a_climate_coefficients <- bind_rows(
  tidy(m_temp_time_overall) %>%
    filter(term == "year") %>%
    mutate(Model = "Spring temperature ~ year"),
  tidy(m_pr_time_overall) %>%
    filter(term == "year") %>%
    mutate(Model = "Spring precipitation ~ year")
) %>%
  transmute(
    Model,
    Predictor = term,
    Beta = round(estimate, 5),
    SE = round(std.error, 5),
    t = round(statistic, 2),
    p = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
  )

m_temp_time_region <- lm(temp_mean_spring ~ year * region_label, data = climate_old)
m_pr_time_region <- lm(pr_sum_spring ~ year * region_label, data = climate_old)

table4b_climate_anova <- bind_rows(
  Anova(m_temp_time_region, type = 2) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Predictor") %>%
    mutate(Model = "Spring temperature ~ year * region"),
  Anova(m_pr_time_region, type = 2) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Predictor") %>%
    mutate(Model = "Spring precipitation ~ year * region")
) %>%
  transmute(
    Predictor,
    SumSq = round(`Sum Sq`, 3),
    Df = Df,
    F_value = ifelse(is.na(`F value`), NA, round(`F value`, 2)),
    p = ifelse(is.na(`Pr(>F)`), NA, ifelse(`Pr(>F)` < 0.001, "<0.001", sprintf("%.3f", `Pr(>F)`))),
    Model
  )

write.csv2(
  table4a_climate_coefficients,
  file.path(output_dir, "Table_4a_climate_trend_coefficients_pre1960.csv"),
  row.names = FALSE
)

write.csv2(
  table4b_climate_anova,
  file.path(output_dir, "Table_4b_climate_trend_ANOVA_pre1960.csv"),
  row.names = FALSE
)


--------------------------------------------
# 17. Figure 4 and Table 5a-b: Spring temperature versus precipitation
--------------------------------------------
p_fig4_temp_pr <- ggplot(
  climate_old,
  aes(x = temp_mean_spring, y = pr_sum_spring, color = region_label)
) +
  geom_point(alpha = 0.65, size = 1.8) +
  geom_smooth(
    aes(group = 1),
    method = "lm",
    se = TRUE,
    color = "black",
    linewidth = 0.9
  ) +
  scale_color_manual(values = region_cols) +
  labs(
    x = "Mean spring temperature (°C)",
    y = "Spring precipitation sum (mm)",
    color = "Region"
  ) +
  theme_bee()

ggsave(
  file.path(output_dir, "Figure_4_temperature_precipitation_relationship.pdf"),
  p_fig4_temp_pr,
  width = 7,
  height = 5
)

m_temp_pr_overall <- lm(pr_sum_spring ~ temp_mean_spring, data = climate_old)

table5a_temp_pr_coefficients <- tidy(m_temp_pr_overall) %>%
  mutate(Model = "Spring precipitation ~ spring temperature") %>%
  transmute(
    Model,
    Predictor = term,
    Beta = round(estimate, 5),
    SE = round(std.error, 5),
    t = round(statistic, 2),
    p = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
  )

m_temp_pr_region <- lm(pr_sum_spring ~ temp_mean_spring * region_label, data = climate_old)

table5b_temp_pr_anova <- Anova(m_temp_pr_region, type = 2) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Predictor") %>%
  mutate(Model = "Spring precipitation ~ spring temperature * region") %>%
  transmute(
    Predictor,
    SumSq = round(`Sum Sq`, 3),
    Df = Df,
    F_value = ifelse(is.na(`F value`), NA, round(`F value`, 2)),
    p = ifelse(is.na(`Pr(>F)`), NA, ifelse(`Pr(>F)` < 0.001, "<0.001", sprintf("%.3f", `Pr(>F)`))),
    Model
  )

write.csv2(
  table5a_temp_pr_coefficients,
  file.path(output_dir, "Table_5a_temperature_precipitation_coefficients.csv"),
  row.names = FALSE
)

write.csv2(
  table5b_temp_pr_anova,
  file.path(output_dir, "Table_5b_temperature_precipitation_ANOVA.csv"),
  row.names = FALSE
)


--------------------------------------------
# 18. Table 6a and Figure 5: ITD versus mean spring temperature
--------------------------------------------

m_temp_h <- lmer(
  ITDaverage ~ temp_c * Species + (1 | year_f) + (1 | region_label),
  data = bee_mod,
  REML = FALSE,
  control = lmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 100000)
  )
)

pred_temp_h <- ggpredict(
  m_temp_h,
  terms = c("temp_c [all]", "Species")
) %>%
  as.data.frame() %>%
  mutate(
    temp_mean_spring = x + mean_temp,
    Species = group
  )

p_fig5_temp <- ggplot() +
  geom_point(
    data = bee_mod,
    aes(x = temp_mean_spring, y = ITDaverage, color = Species),
    alpha = 0.55,
    size = 1.7
  ) +
  geom_ribbon(
    data = pred_temp_h,
    aes(x = temp_mean_spring, ymin = conf.low, ymax = conf.high, fill = Species),
    alpha = 0.18,
    inherit.aes = FALSE
  ) +
  geom_line(
    data = pred_temp_h,
    aes(x = temp_mean_spring, y = predicted, color = Species),
    linewidth = 1
  ) +
  facet_wrap(~ Species, scales = "free_y", labeller = species_facet_labs) +
  scale_color_manual(values = species_cols) +
  scale_fill_manual(values = species_cols) +
  labs(
    x = "Mean spring temperature (°C)",
    y = "Inter-tegular distance (mm)",
    color = "Species",
    fill = "Species"
  ) +
  theme_bee() +
  theme(legend.position = "none")

ggsave(
  file.path(output_dir, "Figure_5_ITD_temperature_hierarchical.pdf"),
  p_fig5_temp,
  width = 8,
  height = 6
)


--------------------------------------------
# 19. Table 6b and Figure 6: ITD versus spring precipitation
--------------------------------------------

m_pr_h <- lmer(
  ITDaverage ~ pr_c * Species + (1 | year_f) + (1 | region_label),
  data = bee_mod,
  REML = FALSE,
  control = lmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 100000)
  )
)

pred_pr_h <- ggpredict(
  m_pr_h,
  terms = c("pr_c [all]", "Species")
) %>%
  as.data.frame() %>%
  mutate(
    pr_sum_spring = x + mean_pr,
    Species = group
  )

p_fig6_pr <- ggplot() +
  geom_point(
    data = bee_mod,
    aes(x = pr_sum_spring, y = ITDaverage, color = Species),
    alpha = 0.55,
    size = 1.7
  ) +
  geom_ribbon(
    data = pred_pr_h,
    aes(x = pr_sum_spring, ymin = conf.low, ymax = conf.high, fill = Species),
    alpha = 0.18,
    inherit.aes = FALSE
  ) +
  geom_line(
    data = pred_pr_h,
    aes(x = pr_sum_spring, y = predicted, color = Species),
    linewidth = 1
  ) +
  facet_wrap(~ Species, scales = "free_y", labeller = species_facet_labs) +
  scale_color_manual(values = species_cols) +
  scale_fill_manual(values = species_cols) +
  labs(
    x = "Spring precipitation sum (mm)",
    y = "Inter-tegular distance (mm)",
    color = "Species",
    fill = "Species"
  ) +
  theme_bee() +
  theme(legend.position = "none")

ggsave(
  file.path(output_dir, "Figure_6_ITD_precipitation_hierarchical.pdf"),
  p_fig6_pr,
  width = 8,
  height = 6
)


--------------------------------------------
# 20. Export species-specific hierarchical slopes: Tables 3, 6a, 6b
--------------------------------------------

extract_species_slopes <- function(model, var, model_name, predictor_label) {
  trend_column <- paste0(var, ".trend")

  summary(
    emtrends(
      model,
      specs = "Species",
      var = var
    ),
    infer = c(TRUE, TRUE)
  ) %>%
    as.data.frame() %>%
    mutate(
      Model = model_name,
      Predictor = predictor_label,
      Beta = .data[[trend_column]],
      p_adj = p.adjust(p.value, method = "BH")
    ) %>%
    transmute(
      Model,
      Species,
      Predictor,
      Beta = round(Beta, 5),
      SE = round(SE, 5),
      df = round(df, 1),
      t = round(t.ratio, 2),
      p = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value)),
      p_adj = ifelse(p_adj < 0.001, "<0.001", sprintf("%.3f", p_adj))
    )
}

table3_year <- extract_species_slopes(
  model = m_year_h,
  var = "year_c",
  model_name = "ITD ~ year",
  predictor_label = "year"
)

table6a_temp <- extract_species_slopes(
  model = m_temp_h,
  var = "temp_c",
  model_name = "ITD ~ spring temperature",
  predictor_label = "temp_mean_spring"
)

table6b_pr <- extract_species_slopes(
  model = m_pr_h,
  var = "pr_c",
  model_name = "ITD ~ spring precipitation",
  predictor_label = "pr_sum_spring"
)

write.csv2(
  table3_year,
  file.path(output_dir, "Table_3_ITD_year_hierarchical.csv"),
  row.names = FALSE
)

write.csv2(
  table6a_temp,
  file.path(output_dir, "Table_6a_ITD_temperature_hierarchical.csv"),
  row.names = FALSE
)

write.csv2(
  table6b_pr,
  file.path(output_dir, "Table_6b_ITD_precipitation_hierarchical.csv"),
  row.names = FALSE
)


--------------------------------------------
# 21. Table 7 and Figure 7: Regional models for the two best-sampled species
--------------------------------------------

bee_region_focus_h <- bee_mod %>%
  filter(Species %in% c("Bombus lapidarius", "Bombus pascuorum")) %>%
  mutate(
    Species = droplevels(Species),
    region_label = droplevels(region_label)
  )

m_region_year_h <- lmer(
  ITDaverage ~ year_c + Species + region_label + (1 | year_f),
  data = bee_region_focus_h,
  REML = FALSE
)

m_region_temp_h <- lmer(
  ITDaverage ~ temp_c + Species + region_label + (1 | year_f),
  data = bee_region_focus_h,
  REML = FALSE
)

m_region_pr_h <- lmer(
  ITDaverage ~ pr_c + Species + region_label + (1 | year_f),
  data = bee_region_focus_h,
  REML = FALSE
)

make_region_anova_table <- function(model, model_name) {
  anova(model, type = 2) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Predictor") %>%
    mutate(
      Model = model_name,
      F = round(`F value`, 2),
      p = ifelse(`Pr(>F)` < 0.001, "<0.001", sprintf("%.3f", `Pr(>F)`))
    ) %>%
    transmute(
      Model,
      Predictor,
      Num_df = NumDF,
      Den_df = round(DenDF, 2),
      F,
      p
    )
}

table7_region <- bind_rows(
  make_region_anova_table(
    m_region_year_h,
    "ITD ~ year + species + region"
  ),
  make_region_anova_table(
    m_region_temp_h,
    "ITD ~ spring temperature + species + region"
  ),
  make_region_anova_table(
    m_region_pr_h,
    "ITD ~ spring precipitation + species + region"
  )
)

write.csv2(
  table7_region,
  file.path(output_dir, "Table_7_regional_hierarchical_models.csv"),
  row.names = FALSE
)

# Figure: regional ITD patterns using precipitation regional model
emm_region_h <- emmeans(
  m_region_pr_h,
  ~ Species | region_label
) %>%
  as.data.frame()

p_fig7_region <- ggplot() +
  geom_jitter(
    data = bee_region_focus_h,
    aes(x = region_label, y = ITDaverage, color = Species),
    alpha = 0.35,
    size = 1.5,
    position = position_jitterdodge(
      jitter.width = 0.15,
      dodge.width = 0.6
    )
  ) +
  geom_pointrange(
    data = emm_region_h,
    aes(
      x = region_label,
      y = emmean,
      ymin = lower.CL,
      ymax = upper.CL,
      color = Species
    ),
    position = position_dodge(width = 0.6),
    linewidth = 0.8,
    size = 0.8
  ) +
  scale_color_manual(values = species_cols) +
  labs(
    x = "Region",
    y = "Inter-tegular distance (mm)",
    color = "Species"
  ) +
  theme_bee()

ggsave(
  file.path(output_dir, "Figure_7_regional_ITD_hierarchical.pdf"),
  p_fig7_region,
  width = 7,
  height = 5
)


--------------------------------------------
# 22. Optional exploratory model: year, temperature and precipitation together
--------------------------------------------

m_complex <- lm(
  ITDaverage ~ year + temp_mean_spring + pr_sum_spring + Species,
  data = bee_clim
)

complex_summary <- tidy(m_complex)
complex_anova <- Anova(m_complex, type = 2) %>% as.data.frame()
climate_correlations <- cor(
  bee_clim[, c("year", "temp_mean_spring", "pr_sum_spring")],
  use = "complete.obs"
)
complex_vif <- car::vif(m_complex)

write.csv2(
  complex_summary,
  file.path(output_dir, "Appendix_complex_model_coefficients.csv"),
  row.names = FALSE
)

write.csv2(
  complex_anova,
  file.path(output_dir, "Appendix_complex_model_ANOVA.csv"),
  row.names = TRUE
)

write.csv2(
  as.data.frame(climate_correlations),
  file.path(output_dir, "Appendix_climate_correlations.csv"),
  row.names = TRUE
)

write.csv2(
  as.data.frame(complex_vif),
  file.path(output_dir, "Appendix_complex_model_VIF.csv"),
  row.names = TRUE
)


--------------------------------------------
# 23. Model summaries and diagnostics
--------------------------------------------

model_summaries <- list(
  global_species = summary(m_species),
  year_hierarchical = summary(m_year_h),
  temperature_hierarchical = summary(m_temp_h),
  precipitation_hierarchical = summary(m_pr_h),
  regional_year = summary(m_region_year_h),
  regional_temperature = summary(m_region_temp_h),
  regional_precipitation = summary(m_region_pr_h)
)

sink(file.path(output_dir, "model_summaries.txt"))
print(model_summaries)
cat("\n\nSession info:\n")
print(sessionInfo())
sink()

END
