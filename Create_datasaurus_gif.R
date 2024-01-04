library(datasauRus)
library(gganimate)


  summary_stat = datasaurus_dozen %>% 
    group_by(dataset) %>% 
    summarize(
      mean_x    = mean(x),
      mean_y    = mean(y),
      std_dev_x = sd(x),
      std_dev_y = sd(y),
      corr_x_y  = cor(x, y)
    )

  text_sf = summary_stat %>%
    mutate(stats_text = paste0("mean_x: ",  round(mean_x,4),"\n",
                               "mean_y: ", round(mean_y,4),"\n",
                               "sd_x: ",round(std_dev_x,4),"\n",
                               "sd_y: ", round(std_dev_y,4),"\n",
                               "corr_x_y: ", round(corr_x_y,4),"\n")) %>%
    select(dataset, stats_text)
                               
                               
                               
  datasaurus_dozen = left_join(datasaurus_dozen, text_sf, by ="dataset")

p = ggplot(datasaurus_dozen, aes(x = x, y = y, color = dataset, group =1L))+
  geom_point(size = 2) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  transition_states(states = dataset,
                    transition_length = 2,
                    state_length = 1) +
  ease_aes('cubic-in-out') +
  enter_fade() + 
  ggtitle(label = 'Now showing dataset: {closest_state}',
          subtitle = "Summary Stats
          :\n{datasaurus_dozen[datasaurus_dozen$dataset == {closest_state},4][1,1]}")

animate(p, fps = 12, duration = 13, height = 5.5, width = 4, units ="in", res = 300)
