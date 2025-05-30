


#' edviz Real Recurring Revenue Per Pupil
#'
#'
#' @export
edviz_rev_recur_real_pp <- function(inflation.index.year, fund = 11){
  
  df <- tt_import_fid_R() %>% 
    filter(fund %in% {{ fund }},
           majorclass < 600,
           suffix !=250
           ) %>% 
    group_by(FY) %>% 
    summarise(amount.rev = sum(amount))%>% 
    tt_inf_deflate(value.var = amount.rev, 
                   year.var = FY, 
                   index.year = {{ inflation.index.year }}, 
                   deflator = "SLIPD") %>% 
    # rename(amount.rev.SLIPD = paste0("amount.rev.SLIPD.", {{ inflation.index.year}})) %>% 
    left_join(tt_cy_state_pupil()) %>% 
    mutate(amount.rev.SLIPD.pp = amount.rev.SLIPD / pupilcnt)
  
  
  df %>% 
    ggplot(aes(FY, amount.rev.SLIPD.pp)) +
    geom_line()+
    geom_point()+
    ggplot2::scale_x_continuous(n.breaks = (max(df$FY) - min(df$FY)))+
    ggplot2::scale_y_continuous(labels = dollar_format())+
    theme_classic()+
    labs(title = "Per Pupil Recurring Education Revenue",
         subtitle = "Inflation Adjusted (S&L IPD), Federal Stimulus Removed",
         caption = "<i>Data Source: Financial Information Database (FID) 
         and State Aid Financial Status Report</i>",
         x = "",
         y = "")+
    theme(plot.caption = ggtext::element_markdown())
  
  
  
}


edviz_rev_recur_real_pp(2024)

 