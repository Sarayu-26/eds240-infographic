# Check the exact unique labels in your raw fleet data
fleet %>% 
  distinct(geartype_label) %>% 
  arrange(geartype_label)

fleet %>% 
  filter(geartype_label %in% c("Pots and Traps", "Set Gillnets")) %>%
  summarise(
    total_hours         = sum(hours, na.rm = TRUE),
    total_fishing_hours = sum(fishing_hours, na.rm = TRUE)
  )


mean_hours_gear <- fleet %>%
  filter(geartype_label != "Fishing") %>%
  group_by(geartype_label, year) %>%
  summarise(annual_hours = sum(fishing_hours, na.rm = TRUE), .groups = "drop") %>%
  group_by(geartype_label) %>%
  summarise(mean_hours = mean(annual_hours, na.rm = TRUE))

fleet_agg_gear <- fleet_agg_gear %>%
  mutate(geartype_label = fct_reorder(geartype_label, total_fishing_hours))

nrow(fleet)


fleet %>% 
  filter(str_detect(geartype_label, "Fishing")) %>%
  distinct(geartype_label)

fleet %>%
  filter(geartype_label == "Pole\nand\nLine") %>%
  summarise(
    fishing_hours = sum(fishing_hours, na.rm = TRUE),
    hours         = sum(hours, na.rm = TRUE)
  )
