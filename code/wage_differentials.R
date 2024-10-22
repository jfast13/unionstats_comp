##Calculating metrics for the table 
wage_results <- union_master %>%
  filter(!is.na(unmem), emp==1) %>% 
  group_by(year) %>%
  summarise(
    Sample_Size = n(),
    Wage = weighted.mean(wage, w = wgt, na.rm = TRUE),
    Union_Coverage = weighted.mean(union, w=wgt, na.rm = TRUE),
    Union_Wage = weighted.mean(wage[unmem == 1], w = wgt[unmem == 1], na.rm = TRUE),
    Nonunion_Wage = weighted.mean(wage[unmem == 0], w = wgt[unmem == 0], na.rm = TRUE),
    Unadjusted_Union_Wage_Prem = (Union_Wage - Nonunion_Wage) / Nonunion_Wage * 100,
    .groups = "drop"
  )

# View the results
wage_resutls <- wage_results[c("Sample_Size", "Wage", "Union_Wage", "Nonunion_Wage", "Unadjusted_Union_Wage_Prem", "Union_Coverage")]
print(wage_results)
write.xlsx(wage_results, "output/union_wage_results.xlsx", rowNames = FALSE)
