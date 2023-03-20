

library(tidyverse)
library(TannersTools)
tt_dir_rd()


# Enrollment Trend
tt_import_cy_data(8) %>% 
  mutate(dnum = as.numeric(dcode)) %>% 
  group_by(FY) %>% 
  summarise(pupilcnt = sum(pupilcnt, na.rm = TRUE)) %>% 
  ggplot(aes(FY, pupilcnt))+
  geom_line()+
  geom_point()+
  scale_x_continuous(n.breaks = (max(cydata_state$FY) - min(cydata_state$FY)))+
  scale_y_continuous(labels = comma_format())+
  theme_classic()+
  labs(title = "State Aid Membership",
       caption = "Data Source: State aid Financial Status Report",
       x = "",
       y = "")



