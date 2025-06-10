
# Effective Revenue

# this script is designed to calculate the effective revenue trend for school
  # districts in Michigan. To do this, I assembled a full panel of 
  # revenue data. Then, I removed UAAL debt payments and adjusted for inflation.



# Setup ------------------------------------------------------------------------

library(tidyverse)
library(TannersTools)
library(scales)
library(gghighlight)
tt_dir_data()


# General Fund -----------------------------------------------------------------

## Form B GF -------------------------------------------------------------------


fbr <- tt_import_formb_fbrev()

# Notes for later
# gloc_total is probably general fund local revenue
# gst_total is probably gneral fund state revenue 

gfrev_1994_2003 <- 
  fbr %>% 
  group_by(FY, dnum) %>% 
  summarise(gfrev = sum(g_total, na.rm = TRUE))




## FID GF ----------------------------------------------------------------------

fidr <- tt_import_fid_R()

gfrev_2004_2024 <- 
  fidr %>% 
  filter(fund == 11,
         suffix != 250, # covid federal stimulus
         suffix != 230, # great recession federal stimulus
         majorclass < 600 # removing inter-fund transfers. Otherwise there's duplication of revenue 
  ) %>% 
  group_by(FY, dnum) %>% 
  summarise(gfrev = sum(amount))



## Combining GF ----------------------------------------------------------------
gfrev <- 
  gfrev_1994_2003 %>% 
  bind_rows(gfrev_2004_2024)

rm(fbr, gfrev_1994_2003, gfrev_2004_2024)


# Special Education Fund -------------------------------------------------------

## Form B SE -------------------------------------------------------------------

fbrs <- tt_import_formb_fbsrev()

sfrev_1994_2003 <- 
  fbrs %>% 
  group_by(FY, dnum) %>% 
  summarise(sfrev = sum(s_total, na.rm = TRUE))


## FID SE ----------------------------------------------------------------------

sfrev_2004_2024 <- 
  fidr %>% 
  filter(fund == 22,
         suffix != 250, # covid federal stimulus
         suffix != 230, # great recession federal stimulus
         majorclass < 600 # removing inter-fund transfers. Otherwise there's duplication of revenue 
  ) %>% 
  group_by(FY, dnum) %>% 
  summarise(sfrev = sum(amount))

# Combining SE -----------------------------------------------------------------
sfrev <- 
  sfrev_1994_2003 %>% 
  bind_rows(sfrev_2004_2024)


rm(fbrs, sfrev_1994_2003, fidr, sfrev_2004_2024)

# Combining GF and SE ----------------------------------------------------------
rev <- 
  gfrev %>% 
  left_join(sfrev, by = join_by(FY, dnum)) %>% 
  mutate(gfrev = ifelse(is.na(gfrev), 0, gfrev),
         sfrev = ifelse(is.na(sfrev), 0, sfrev),
         rev = gfrev + sfrev)

rm(gfrev, sfrev)


# Student Count ----------------------------------------------------------------
library(MIEdData)
selected_directory <- "C:/Users/Tdelpier/vault/projects/test3"
msds_hc <- msds_hc_import()
  # this does not include PK. I need a way to include PK for each year that it exists. 
  # I believe this will require a bunch of work in the mieddata package 

msds_hc_pk <- 
  msds_hc_pk_import() %>% 
  rename(students.pk = pktotl) %>% 
  select(FY, dnum, students.pk)

students <- 
  msds_hc %>% 
  rename(students.k12 = totall) %>% 
  select(FY, dnum, students.k12) %>% 
  full_join(msds_hc_pk, by = join_by(FY, dnum)) %>% 
  mutate(students.pk = ifelse(is.na(students.pk), 0, students.pk),
         students.k12 = ifelse(is.na(students.k12), 0, students.k12),
         students.pk12 = students.pk + students.k12)

rm(msds_hc)


# MPSERS UAAL Cost

## Form B Salary ---------------------------------------------------------------
sal_1995_2003 <- 
  tt_import_formb_fbgfe() %>% 
  filter(bcode == "0000") %>% 
  mutate(sal = g_psal + g_npsal) %>% 
  select(FY, dcode, dnum, sal) %>%
  group_by(FY, dnum) %>%
  summarise(sal = sum(sal))


## FID Salary ------------------------------------------------------------------
sal_2004_2024 <- 
  tt_import_fid_E() %>% 
  filter(fund == 11,
         object.1 == 1000) %>%
  group_by(FY, dnum) %>%
  summarise(sal = sum(amount))


## District UAAL ---------------------------------------------------------------

mpsers.stablization.rates <- 
  read.csv(tt_dir_vault("topics/MPSERS/MPSERS_Rates/MPSERS_Rates.csv"))

uaal_district <- 
  sal_1995_2003 %>% 
  bind_rows(sal_2004_2024) %>% 
  left_join(mpsers.stablization.rates, by = join_by(FY)) %>% 
  mutate(district.mpsers.uaal.rate = as.numeric(district.mpsers.uaal.rate),
         district.mpsers.uaal.rate = ifelse(is.na(district.mpsers.uaal.rate), 0 , district.mpsers.uaal.rate),
         uaal.district = district.mpsers.uaal.rate * sal) %>% 
  select(FY, dnum, uaal.district)

rm(sal_1995_2003, sal_2004_2024, mpsers.stablization.rates)
## State UAAL ------------------------------------------------------------------

  # icd 573 = 147c(1) UAAL stabilization
  # icd 583 = 147c(2) UAAL one-time deposit

uaal_state <- 
  tt_import_cy_allow(8) %>% 
  mutate(icd = as.numeric(icd)) %>% 
  filter(icd %in% c(573, 583)) %>% 
  select(FY, dnum, icd, amount) %>% 
  pivot_wider(id_cols = c(FY, dnum), 
              names_prefix = "icd.", names_from = icd, 
              values_from = amount) %>% 
  group_by(FY, dnum) %>% 
  summarise(sec.147c1 = sum(icd.573, na.rm = TRUE),
            sec.147c2 = sum(icd.583, na.rm = TRUE)) %>% 
  mutate(uaal.state = sec.147c1 + sec.147c2) %>% 
  select(FY, dnum, uaal.state)



## Total UAAL ------------------------------------------------------------------
uaal_total <- 
  uaal_district %>% 
  left_join(uaal_state, by = join_by(FY, dnum)) %>% 
  mutate(uaal.district = ifelse(is.na(uaal.district), 0, uaal.district),
         uaal.state = ifelse(is.na(uaal.state), 0, uaal.state),
         uaal.total = uaal.district + uaal.state)


rm(uaal_district, uaal_state)


# Combining --------------------------------------------------------------------

data_effective_rev <- 
  rev %>% 
  full_join(students, by = join_by(FY, dnum)) %>% 
  full_join(uaal_total, by = join_by(FY, dnum)) %>% 

  tt_inf_deflate(value.var = rev, year.var = FY, index.year = 2024,
                 deflator = "SLIPD") %>% 
  rename(rev.nom = rev,
         rev.real = rev.SLIPD) %>% 
  mutate(uaal.total.real = uaal.total / index.SLIPD,
         rev.effective = rev.real - uaal.total.real)



# Figures ----------------------------------------------------------------------

data_effective_rev_state <- 
  data_effective_rev %>% 
  group_by(FY) %>% 
  filter(FY >= 1995) %>% 
  summarise(rev.nom = sum(rev.nom, na.rm = TRUE),
            rev.real = sum(rev.real, na.rm = TRUE),
            rev.effective = sum(rev.effective, na.rm = TRUE),
            students.pk12 = sum(students.pk12, na.rm = TRUE)) %>% 
  mutate(rev.nom.pp = rev.nom / students.pk12,
         rev.real.pp = rev.real / students.pk12,
         rev.effective.pp = rev.effective / students.pk12) 


data_effective_rev_state %>% 
  ggplot(aes(x = FY)) +
  geom_line(aes(y = rev.nom), color = "red") +
  geom_line(aes(y = rev.real), color = "blue") +
  geom_line(aes(y = rev.effective), color = "green") +
  theme_classic()
  
data_effective_rev_state %>% 
  ggplot(aes(FY, rev.effective / 1000000000)) +
  geom_line(size = 1) +
  theme_classic() +
  scale_y_continuous(labels = dollar_format()) +
  scale_x_continuous(breaks = seq(from = 1995, to = 2025, by = 5)) +
  labs(title = "Effective PK-12 Education Revenue",
       subtitle = "FY 1995 - 2024",
       y = "Billion",
       x = "Fiscal Year",
       caption = 
"Notes-- 
Data source:  Form B and the Financial Information Database (FID).
Revenue:       Includes all local General Fund and Special Education fund revenue. 
Exclusions:    Onetime federal stimulus funding is excluded. Both the state and local share of the MPSERS UAAL paid through school districts is excluded.
Institutions:    Includes publicly operated districts (LEAs), charter school districts (PSAs), and intermediate school districts (ISDs).
Inflation:         Adjusted for inflation using the State and Local Government Implicit Price Deflator (S&L IPD)."
       ) +
  theme(plot.caption = element_text(hjust = 0))



data_effective_rev_state %>% 
  ggplot(aes(FY, rev.effective.pp)) +
  geom_line(size = 1) +
  theme_classic() +
  scale_y_continuous(labels = dollar_format()) +
  scale_x_continuous(breaks = seq(from = 1995, to = 2025, by = 5)) +
  labs(title = "Effective PK-12 Education Revenue Per Pupil",
       subtitle = "FY 1995 - 2024",
       y = "Per Pupil",
       x = "Fiscal Year",
       caption = 
         "Notes-- 
Data source:  Form B, the Financial Information Database (FID), and the Michigan Student Data System (MSDS).
Revenue:       Includes all local General Fund and Special Education fund revenue. 
Exclusions:    Onetime federal stimulus funding is excluded. Both the state and local share of the MPSERS UAAL paid through school districts is excluded.
Students:       Pupils is measured by the Fall pupil K-12 and PK head count. 
Institutions:    Includes publicly operated districts (LEAs), charter school districts (PSAs), and intermediate school districts (ISDs).
Inflation:         Adjusted for inflation using the State and Local Government Implicit Price Deflator (S&L IPD)."
  ) +
  theme(plot.caption = element_text(hjust = 0))

  











