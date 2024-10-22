## Union Density Table ##

##Calculating all the metrics in the table 
density_results <- union_master %>%
  filter(emp == 1, !is.na(unmem)) %>%
  group_by(year) %>%
  summarise(
    Sample_size = n(),
    Employment = sum(wgt, na.rm = TRUE)/1000,
    Members = sum(wgt[unmem == 1], na.rm = TRUE)/1000 ,
    Covered = sum(wgt[union == 1], na.rm = TRUE)/1000 ,) %>%
  mutate(Percent_members = (Members / Employment) * 100,
         Percent_covered = (Covered / Employment) * 100)

# View the results
print(density_results)
write.xlsx(density_results, "output/union_density_results.xlsx", rowNames = FALSE)

