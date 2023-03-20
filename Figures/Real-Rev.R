





# total revenue per pupil adjusted for inflation 

tt_import_fid_R() %>% 
  filter(fund == 11,
         majorclass < 600,
         suffix !=250 ) %>% 
  group_by(FY) %>% 
  summarise(amount.rev = sum(amount)) %>% 
  tt_inf_deflate(amount.rev, FY, 2022, deflator = "slipd") %>% 
  left_join(cydata_state) %>% 
  mutate(amount.rev.slipd.2022.pp = amount.rev.slipd.2022 / pupilcnt) %>% 
  ggplot(aes(FY, amount.rev.slipd.2022.pp)) +
  geom_line()+
  geom_point()+
  scale_x_continuous(n.breaks = (max(fidr_state$FY) - min(fidr_state$FY)))+
  scale_y_continuous(labels = dollar_format())+
  theme_classic()+
  labs(title = "Per Pupil Recurring Education Revenue",
       subtitle = "Inflation Adjusted (S&L IPD), Federal Stimulus Removed",
       x = "",
       y = "")




