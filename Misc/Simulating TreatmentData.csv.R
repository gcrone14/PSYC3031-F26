##### Simulating TreatmentData.csv #####
library(tidyverse)

set.seed(123)

TreatmentData <- tibble(
  ID = 1:200,
  Age = sample(18:65, 200, replace = TRUE),
  Sex = sample(c("Male", "Female"), 200, replace = TRUE),
  Improvement = rnorm(200, 0, 1.5),
  Depress_Time1 = rnorm(200, 15, 2),
  Depress_Time2 = Depress_Time1 - 2 - Improvement + rnorm(200, 0, 1),
  Depress_Time3 = Depress_Time1 - 5 - 2*Improvement + rnorm(200, 0, 1)
) |>
  mutate(across(Depress_Time1:Depress_Time3, ~ round(pmax(1, pmin(20, .x))))) |>
  select(-Improvement)
  

write_csv(TreatmentData, "Data/TreatmentData.csv")
