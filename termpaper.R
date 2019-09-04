library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(grid)
library(xlsx)
library(gridExtra)
library(lubridate)
library(staplr)


# Getting allthe campaigns in one with for-loop
filenames <- dir(pattern = "*.csv")
facebook_ads_data<- tibble()

for (i in 1:length(filenames)) {
  facebook_ads_data <- read_csv(filenames[i])%>%
    select(-Starts, -Ends, -"Reporting starts", -"Reporting ends") %>%
    rename(ad_name = "Ad name",
           ad_set_name = "Ad set name",
           day = Day,
           ad_delivery = "Ad delivery",
           reach = Reach,
           impressions = Impressions,
           frequency = Frequency,
           result_type = "Result Type",
           results = Results,
           cost_per_result = "Cost per result",
           amount_spent = "Amount spent (RUB)",
           CPM = "CPM (cost per 1,000 impressions)",
           link_clicks = "Link clicks",
           CPC = "CPC (cost per link click)",
           CTR = "CTR (link click-through rate)",
           CPC_all = "CPC (all)",
           CTR_all = "CTR (all)",
           clicks_all = "Clicks (all)",
           ad_ID = "Ad ID",
           campaign_ID = "Campaign ID"
    ) %>%
    bind_rows(facebook_ads_data)
}

# It is a common practice for the SMM managers to duplicate ad sets to increase the overall reach of the campaign
# As far as we have the russian data such ad sets are marked as "* — Копия"
# Ad sets as such are the same in terms of settings but could have been duplicated in different time periods
# So we can't just combine them, cause it will affect the day by day analysis
 
facebook_ads_data$ad_set_name <- gsub ("— Копия","", facebook_ads_data$ad_set_name)
grep(".— Копия", facebook_ads_data$ad_set_name)
adsetnames <- unique(facebook_ads_data$ad_set_name)

# The following loop will provide the ovearall summary of our campaigns and analysis for every campaign

for (i in 1:length(adsetnames)) {
  
  temp_df_summary <- facebook_ads_data %>%
    filter(ad_set_name == adsetnames[i],
           impressions != 0) %>%
    group_by(ad_name) %>%
    summarise(Impressions = sum(impressions),
              "Amount Spent (RUB)" = sum(amount_spent, na.rm = TRUE),
              CPM = (sum(amount_spent, na.rm = TRUE) / sum(impressions, na.rm = TRUE)) * 1000,
              Clicks = sum(link_clicks, na.rm = TRUE),
              CPC = (sum(amount_spent, na.rm = TRUE) / sum(link_clicks, na.rm = TRUE)),
              Leads = sum(results, na.rm = TRUE),
              CPL = (sum(amount_spent, na.rm = TRUE) /sum(results, na.rm = TRUE)))
  
  temp_df_summary <- rbind(temp_df_summary, tibble(ad_name = "Total", 
                                                   Impressions = sum(temp_df_summary$Impressions, na.rm = TRUE),
                                                   "Amount Spent (RUB)" = sum(temp_df_summary$`Amount Spent (RUB)`, na.rm = TRUE),
                                                   CPM = mean(temp_df_summary$CPM, na.rm = TRUE),
                                                   Clicks = sum(temp_df_summary$Clicks, na.rm = TRUE),
                                                   CPC = sum(`Amount Spent (RUB)`, na.rm = TRUE)/sum(Clicks, na.rm = TRUE),
                                                   Leads = sum(temp_df_summary$Leads, na.rm = TRUE),
                                                   CPL = sum(`Amount Spent (RUB)`, na.rm = TRUE)/sum(Leads, na.rm = TRUE))) %>%
    arrange(-Impressions)
  
  plot_impressions <- 
    facebook_ads_data %>%
    filter(ad_set_name == adsetnames[i],
           impressions != 0) %>%
    group_by(day) %>%
    summarise(Impressions = sum(impressions, na.rm = TRUE),
              "Amount Spent (RUB)" = sum(amount_spent, na.rm = TRUE),
              CPM = (sum(amount_spent, na.rm = TRUE) / sum(impressions, na.rm = TRUE)) * 1000,
              Clicks = sum(link_clicks, na.rm = TRUE),
              CPC = (sum(amount_spent, na.rm = TRUE) / sum(link_clicks, na.rm = TRUE)),
              Leads = sum(results, na.rm = TRUE),
              CPL = (sum(amount_spent, na.rm = TRUE) / sum(results, na.rm = TRUE)))%>%
    ggplot(aes(x = day)) +
    geom_col(aes(y = Impressions), fill = "light blue") +
    xlab("Days")+
    theme_classic() 
  
  
  plot_CPM <- 
    facebook_ads_data %>%
    filter(ad_set_name == adsetnames[i],
           impressions != 0) %>%
    group_by(day) %>%
    summarise(Impressions = sum(impressions, na.rm = TRUE),
              "Amount Spent (RUB)" = sum(amount_spent, na.rm = TRUE),
              CPM = (sum(amount_spent, na.rm = TRUE) / sum(impressions, na.rm = TRUE)) * 1000,
              Clicks = sum(link_clicks, na.rm = TRUE),
              CPC = (sum(amount_spent, na.rm = TRUE) / sum(link_clicks, na.rm = TRUE)),
              Leads = sum(results, na.rm = TRUE),
              CPL = (sum(amount_spent, na.rm = TRUE) / sum(results, na.rm = TRUE)))%>%
    ggplot(aes(x = day)) +
    geom_point(aes(y = CPM), color = "steelblue", size =2) +
    geom_text(mapping = aes(y = CPM,
                            label = round(CPM, digits = 0)),
              position = position_nudge(y = +30))+
    geom_line(aes(y = CPM), color = "steelblue", size = 1, linetype = "dashed") +
    ylab("CPM (RUB)")+ xlab("Days") +
    theme_classic()
  
  plot_CPC_CPL <- 
    facebook_ads_data %>%
    filter(ad_set_name == adsetnames[i],
           impressions != 0) %>%
    group_by(day) %>%
    summarise(Impressions = sum(impressions, na.rm = TRUE),
              "Amount Spent (RUB)" = sum(amount_spent, na.rm = TRUE),
              CPM = (sum(amount_spent, na.rm = TRUE) / sum(impressions, na.rm = TRUE)) * 1000,
              Clicks = sum(link_clicks, na.rm = TRUE),
              CPC = (sum(amount_spent, na.rm = TRUE) / sum(link_clicks, na.rm = TRUE)),
              Leads = sum(results, na.rm = TRUE),
              CPL = (sum(amount_spent, na.rm = TRUE) / sum(results, na.rm = TRUE)))%>%
    ggplot(aes(x = day)) +
    geom_point(aes(y = CPC, colour = "CPC"), size = 2) +
    geom_text(mapping = aes(y = CPC,
                            label = round(CPC, digits = 0)),
              position = position_nudge(y = +30))+
    geom_line (aes(y = CPC), color = "blue 4", size = 1, linetype = "dashed") +
    geom_point(aes(y = CPL, colour = "CPL"), size = 2) +
    geom_text(mapping = aes(y = CPL,
                            label = round(CPL, digits = 0)),
              position = position_nudge(y = +30))+
    geom_line (aes(y = CPL), color = "salmon3", size = 1, linetype = "dashed")+
    ylab("RUB") + xlab("Days")+
    scale_colour_manual(values=c("blue 4","salmon3")) +
    labs(colour = "")+
    theme_classic() %+replace%
    theme(legend.position = "bottom",
          legend.direction = "horizontal") 
  
  
  pdf(paste(substring(adsetnames[i], 1,), ".pdf"), height = 11, width = 10)
  text <- textGrob(paste("REPORT: ", "Ad set: ", adsetnames[i]), gp = gpar(fontsize = 20))
  temp_df_gtable <- tableGrob(temp_df_summary)
  grid.arrange(text, temp_df_gtable,plot_impressions, plot_CPM, plot_CPC_CPL, layout_matrix = rbind(c(1, 1), c(2, 2), c(2, 2), c(3, 3), c(3, 3), c(4, 4), c(4, 4), c(5, 5), c(5, 5)))
  dev.off()
}

pdf_combine(input =paste(adsetnames, ".pdf"), output = "all_in_one.pdf")

least_effective_ads <- function(KPI, maximum) {
  for (i in 1:length(adsetnames)) {
    least_effect <- facebook_ads_data %>%
      filter(ad_set_name == adsetnames[i],
             impressions != 0) %>%
      group_by(ad_name) %>%
      summarise(Impressions = sum(impressions),
                "Amount Spent (RUB)" = sum(amount_spent, na.rm = TRUE),
                CPM = (sum(amount_spent, na.rm = TRUE) / sum(impressions, na.rm = TRUE)) * 1000,
                Clicks = sum(link_clicks, na.rm = TRUE),
                CPC = (sum(amount_spent, na.rm = TRUE) / sum(link_clicks, na.rm = TRUE)),
                Leads = sum(results, na.rm = TRUE),
                CPL = (sum(amount_spent, na.rm = TRUE) / sum(results, na.rm = TRUE))) 
    temp_list <- which(least_effect[,KPI] > maximum)
    adtext <- ""
    if (length(temp_list) == 0){
      print(paste("There are no ads with a ", KPI, " over ", maximum, " in ", adsetnames[i]))
    }
    else {
      adtext <- paste("Ad", temp_list, collapse = ", ")
    print(paste("The ads in the", adsetnames[i], "campaign that have a", KPI, "over", maximum, "are:", adtext, sep =" "))
    }
  }
}

least_effective_ads(KPI = "CPL", maximum = 154)


# Some plotting of the key parameters
# -------------

# Combine with term paper

#impressions for Ads at ad_set level on daily basis
daybyday_time_ad_set_impressions <- facebook_ads_data %>%
  group_by(day,ad_set_name) %>% 
  summarise(impressions_day_ad_set = sum(impressions,na.rm = TRUE) )

impression_general <- daybyday_time_ad_set_impressions %>% 
  ggplot(aes(x=day,fill=factor(ad_set_name)))+
  geom_col(aes(y=impressions_day_ad_set),alpha=0.8) +
  ylab("Impressions")+
  labs(fill="")+
  theme_classic() 

#CPM (cost per 1000) for Ads at ad_set level on daily basis
daybyday_time_CPM <- facebook_ads_data %>% 
  group_by(day, ad_set_name) %>%
  summarise(CPM_day_ad_set = sum(impressions*CPM/1000,na.rm = TRUE)/sum(impressions,na.rm = TRUE)*1000)


CPM_general <- daybyday_time_CPM %>% 
  ggplot(aes(x=day,color=factor(ad_set_name)))+
  geom_line(aes(y=CPM_day_ad_set),size=1) +
  geom_point(aes(y=CPM_day_ad_set),size=2)+
  ylab("CPM") +
  labs(color="")+
  theme_classic() 

#CPL (cost per results) for Ads at ad_set level on daily basis
daybyday_time_CPL <- facebook_ads_data %>%
  group_by(day, ad_set_name) %>%
  summarise(CPL_day_ad_set = sum(amount_spent,na.rm = TRUE)/sum(results,na.rm = TRUE))

CPL_general <- daybyday_time_CPL %>% 
  ggplot(aes(x=day,color=factor(ad_set_name)))+
  geom_line(aes(y=CPL_day_ad_set),size=1) +
  geom_point(aes(y=CPL_day_ad_set),size=2)+
  ylab("CPL")+
  labs(color="")+
  theme_classic()


# CPC (cost per click) for Ads at ad_set level on daily basis
daybyday_time_CPC <- facebook_ads_data %>%
  group_by(day, ad_set_name) %>%
  summarise(CPC_day_ad_set = sum(link_clicks*CPC,na.rm = TRUE)/sum(link_clicks,na.rm = TRUE))

CPC_general <- daybyday_time_CPC %>% 
  ggplot(aes(x=day,color=factor(ad_set_name)))+
  geom_line(aes(y=CPC_day_ad_set),size=1) +
  geom_point(aes(y=CPC_day_ad_set),size=2)+
  ylab("CPC")+
  labs(color="")+
  theme_classic()

pdf()
grid.arrange(impression_general,CPM_general,CPL_general,CPC_general, nrow=4,ncol=1, top="Campaigns Comparison")
dev.off()

# Previous version
