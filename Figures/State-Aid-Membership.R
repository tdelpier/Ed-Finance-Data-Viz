

# Enrollment Trend
df <- tt_import_cy_data(8) %>% 
  mutate(dnum = as.numeric(dcode)) %>% 
  group_by(FY) %>% 
  summarise(pupilcnt = sum(pupilcnt, na.rm = TRUE))

df %>% 
  ggplot(aes(FY, pupilcnt))+
  geom_line()+
  geom_point()+
  scale_x_continuous(n.breaks = (max(df$FY) - min(df$FY)))+
  scale_y_continuous(labels = comma_format())+
  theme_classic()+
  labs(title = "State Aid Membership",
       caption = "Data Source: State aid Financial Status Report",
       x = "",
       y = "")



