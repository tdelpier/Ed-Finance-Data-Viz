


F33 <- 
  tt_import_f33() %>% 
  tt_inf_deflate(value.var = TOTALREV, year.var = FY, index.year = 2021, deflator = "SLIPD")


state_compare <-
  F33 %>%
  filter(STABBR != "DC") %>%
  group_by(FY, STABBR) %>%
  summarise(TOTALREV.SLIPD = sum(TOTALREV.SLIPD, na.rm = TRUE),
            TOTALREV = sum(TOTALREV, na.rm = TRUE),
            V33 = sum(V33, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(STABBR, FY) %>%
  mutate(highlight = ifelse(STABBR == "MI", "Michigan", "")) %>%
  group_by(STABBR) %>%
  mutate(TOTALREV.SLIPD_pp = TOTALREV.SLIPD / V33,
         Change_1995 = TOTALREV.SLIPD / TOTALREV.SLIPD[FY == 1995],
         Change_2001 = TOTALREV.SLIPD / TOTALREV.SLIPD[FY == 2001],
         Change_2008 = TOTALREV.SLIPD / TOTALREV.SLIPD[FY == 2008],
         Change_1995_pp = TOTALREV.SLIPD_pp / TOTALREV.SLIPD_pp[FY == 1995],
         Change_2001_pp = TOTALREV.SLIPD_pp / TOTALREV.SLIPD_pp[FY == 2001],
         Change_2008_pp = TOTALREV.SLIPD_pp / TOTALREV.SLIPD_pp[FY == 2008]) %>%
  ungroup() %>%
  group_by(FY) %>%
  mutate(rank_Change_1995 = rank(-Change_1995),
         rank_Change_2001 = rank(-Change_2001),
         rank_Change_2008 = rank(-Change_2008),
         rank_Change_1995_pp = rank(-Change_1995_pp),
         rank_Change_2001_pp = rank(-Change_2001_pp),
         rank_Change_2008_pp = rank(-Change_2008_pp),
         rank_TOTALREV.SLIPD_pp = rank(-TOTALREV.SLIPD_pp))



## 50 State comparison
RevChange_1995_50_State <-
  ggplot()+
  geom_line(data = state_compare, aes(x = FY, y = Change_1995, group = STABBR), alpha = .25, size = .5) +
  geom_line(data = state_compare %>% filter(STABBR == "MI"), aes(x = FY, y = Change_1995), color = "#cc0404", size = 1)+
  geom_hline(aes( yintercept = 1), lty = 2)+
  scale_y_continuous(labels = percent_format())+
  theme_classic()+
  scale_color_manual(values = c("#141517", "#cc0404"))+
  # labs(x = "",
  #      y = "")
  labs(title = "Inflation Adjusted Total K-12 Education Revenue as Percentage of 1995 Revenue",
       subtitle = "50 states, 1995-2021",
       caption = "Source: National F-33 Data",
       x ="",
       y = "")

RevChange_1995_50_State




ggsave(plot = RevChange_1995_50_State,
       filename = "RevChange_1995_50_State.svg",
       width = 9,
       height = 6)



present_1_RevChange_1995_50_State <- ggplot()+
  geom_line(data = state_compare %>% filter(STABBR != "MI"), aes(x = FY, y = Change_1995, group = STABBR), color = "#3F3F3F", alpha = .5, size = .75) +
  # geom_line(data = state_compare %>% filter(STABBR == "MI"), aes(x = FY, y = Change_1995), color = "red", size = 1.5)+
  geom_hline(aes( yintercept = 1), lty = 2, size = .75)+
  scale_y_continuous(labels = percent_format(), limits = c(.8,2.1))+
  theme_classic()+
  scale_color_manual(values = c("#141517", "#cc0404"))+
  labs(title = "Inflation Adjusted Total K-12 Education Revenue as Percentage of 1995 Revenue",
       subtitle = "49 states, 1995-2021",
       caption = "Source: National F-33 Data",
       x ="",
       y = "")

present_1_RevChange_1995_50_State

ggsave(plot = present_1_RevChange_1995_50_State,
       filename = "present_1_RevChange_1995_50_State.svg",
       width = 8,
       height = 4)




present_2_RevChange_1995_50_State <- ggplot()+
  geom_line(data = state_compare %>% filter(STABBR != "MI"), aes(x = FY, y = Change_1995, group = STABBR), color = "#3F3F3F", alpha = .5, size = .75) +
  geom_line(data = state_compare %>% filter(STABBR == "MI"), aes(x = FY, y = Change_1995), color = "#C2002F", size = 1.5)+
  geom_hline(aes( yintercept = 1), lty = 2, size = .75)+
  scale_y_continuous(labels = percent_format(), limits = c(.8,2.1))+
  theme_classic()+
  scale_color_manual(values = c("#141517", "#cc0404"))+
  labs(title = "Inflation Adjusted Total K-12 Education Revenue as Percentage of 1995 Revenue",
       subtitle = "50 states, 1995-2021",
       caption = "Source: National F-33 Data",
       x ="",
       y = "")

present_2_RevChange_1995_50_State

ggsave(plot = present_2_RevChange_1995_50_State,
       filename = "present_2_RevChange_1995_50_State.svg",
       width = 8,
       height = 4)


