## State Level Union Membership ##
density_state <- union_master %>% 
  filter(emp == 1,
         age >= 16,
         year > 1977,
         !is.na(unmem),
         (is.na(mind03) | mind03 > 1),
         (is.na(ind90) | !(ind90 %in% c(10, 11, 12, 20, 30, 31, 32))),  
         (is.na(ind80) | !(ind80 %in% c(10, 11, 20, 21, 30, 31))),  
         (is.na(ind70) | !(ind70 %in% c(17, 18, 19, 27, 28, 29)))) %>%
  group_by(year, statefips) %>%
  summarise(
    Employment = sum(wgt, na.rm = TRUE),
    Members = sum(wgt[unmem == 1], na.rm = TRUE),
    Covered = sum(wgt[union == 1], na.rm = TRUE)
  ) %>%
  mutate(
    Percent_members = (Members / Employment) * 100,
    Percent_covered = (Covered / Employment) * 100
    )


density_state %>%
  select(statefips, year, Percent_members, Percent_covered) %>%
  { 
    state_data <- pivot_wider(., names_from = year, values_from = c(Percent_members, Percent_covered))
    write.xlsx(state_data, file.path("output", "state_data.xlsx"), rowNames = FALSE)
  }

