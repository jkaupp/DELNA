library(rio)
library(Hmisc)
library(magrittr)
library(reshape2)
library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)
library(ggthemes)
library(ggbeeswarm)
library(ggparallel)
library(wesanderson)
library(scales)
library(grid)
library(viridis)
library(rvest)
library(countrycode)
library(broom)

library(stringr)

data_file <- "./data/test results.xlsx"


## Process Screening Test - DELNA ----
delna_class_list <- import(data_file, sheet="DELNA_ID")

delna_address <- import(data_file, sheet="address") %>% 
  rename(studentid = ID) %>% 
  mutate(studentid = as.numeric(studentid))

delna_scores <- import(data_file, sheet="Screening") %>% 
  mutate(studentid = na.locf(studentid)) %>% 
  filter(!is.na(score)) %>% 
  spread(task,score) %>% 
  rename(vocabulary = v27a,
         reading = `spq-us3`) %>% 
  gather(task,score, total_score, reading, vocabulary, -studentid) %>% 
  filter(!is.na(score)) %>% 
  rename(delna_id = studentid) %>% 
  mutate(delna_id = as.numeric(delna_id)) %>% 
  select(delna_id,task,score) %>% 
  dcast(delna_id ~ task, value.var = "score", fun.aggregate = mean) %>% 
  rename(`DELNA_score` = total_score,
          `DELNA_reading` = reading,
          `DELNA_vocabulary` = vocabulary) %>% 
  left_join(delna_class_list,., by="delna_id")

# Process the diagnostic ----

diag_scores <- import(data_file,sheet = "Diagnostic") %>% 
  set_colnames(paste("DIAG", colnames(.), sep = "_")) %>% 
  mutate(`DIAG_Q1_Total` = rowSums(.[2:3])/6*100,
         `DIAG_Q2_Total` = rowSums(.[4:8])/15*100,
         `DIAG_Q3_Total` = rowSums(.[9:14])/18*100,
         `DIAG_Total_Lexicogram.` = rowSums(.[c(3,4,10)])/9*100,
         `DIAG_Total_Speed` = `DIAG_Speed`/3*100,
         `DIAG_Total_Score` = rowSums(.[2:15])/42*100) %>% 
  rename(studentid =`DIAG_studentid`) %>% 
  select(-DIAG_Speed)

# Process the EPT ----

ept_scores <- import(data_file, sheet = "EPT") %>% 
  mutate(`EPT_score` = score )

# Create the Master, all scores & link to addresses ----

master <- left_join(delna_scores,diag_scores, by = "studentid") %>% 
  left_join(., ept_scores, by = "studentid") %>% 
  select(studentid, name, email, `DELNA_score`, `DIAG_Total_Score`, `EPT_score`) %>% 
  left_join(delna_address, by="studentid") %>% 
  mutate(Country = countrycode(Country, "iso3c", "country.name")) %>% 
  mutate(Country = ifelse(Country=="United States", "USA", Country)) %>% 
  select(-`Nbr 1`, -`Nbr 2`, -County, -`Address 1`, -`Address 2`, -`Addr Type`) %>% 
  rename(DIAG_score = DIAG_Total_Score) %>% 
  mutate(in_DIAG = ifelse(is.na(DIAG_score), "No", "Yes"),
         DIAG_cutoff = ifelse(DIAG_score>=70 | is.na(DIAG_score),"Above","Below"),
         DELNA_cutoff = ifelse(DELNA_score>=60 | is.na(DELNA_score),"Above","Below"))

summary <- master %>% 
  summarize(DELNA_mean = mean(DELNA_score, na.rm = TRUE),
            DIAG_mean = mean(DIAG_score, na.rm = TRUE))

# DELNA Beeswarm
ggplot(master, aes(y = DELNA_score, x=0)) +
  geom_segment(aes(y = 60, yend = 60, x=-1, xend=1), alpha = 0.2) +
  geom_jitter(aes(fill = DELNA_cutoff), size = 5, position = position_beeswarm(), pch=21) +
  annotate("text", x = 1.2, y = 60, label = "Cutoff Score") +
  labs(x = NULL, y = "DELNA Score", title = "Distribution of DELNA Scores") +
  scale_fill_manual(name = "Grouping", values = c(wes_palette("Zissou")[1],wes_palette("Zissou")[5]), labels = c("Write EPT","Write Diagnostic & EPT")) +
  theme_tufte(base_size = 30) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom")

ggsave("DELNA Distribution.png", width = 10, height = 10, dpi = 300)

# DIAG vs EPT Beeswarm
ggplot(master, aes(y = DIAG_score, x = factor(EPT_score))) +
  geom_hline(aes(yintercept = 70), alpha = 0.2) +
  geom_point(aes(fill = DIAG_cutoff), pch = 21, size = 8, position = position_beeswarm()) +
  scale_fill_manual(name = "", values = c(wes_palette("Zissou")[1],wes_palette("Zissou")[5]), labels = c("Above Cuttoff","Below Cutoff")) +
  scale_x_discrete(labels=c("F"="Fail", "MP"="Marginal Pass", "P"="Pass"), breaks = c("F","MP","P")) +
  labs(x = "\nEPT Score", y = "Diagnostic Score", title = "Distribution of Diagnostic Scores") +
  theme_tufte(base_size = 20) +
  theme(legend.position = "bottom")

 ggsave("DIAG vs EPT.png", width = 16, height = 10, dpi = 300)

# DELNA vs EPT Beeswarm
ggplot(subset(master, !is.na(EPT_score) & EPT_score != "DNW"), aes(y = DELNA_score, x = 1)) +
  geom_hline(aes(yintercept = 60), alpha = 0.2) +
  geom_point(aes(fill = DELNA_cutoff), pch = 21, size = 4, position = position_beeswarm()) +
  scale_fill_manual(name = "", values = c(wes_palette("Zissou")[1],wes_palette("Zissou")[5]), labels = c("Above Cuttoff","Below Cutoff")) +
  #scale_x_discrete(labels=c("F"="Fail", "MP"="Marginal Pass", "P"="Pass"), breaks = c("F","MP","P")) +
  labs(x = "\nEPT Score", y = "DELNA Score", title = "DELNA & EPT Performance") +
  theme_tufte(base_size = 20) +
  facet_wrap(~EPT_score) +
  theme(axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      legend.position = "bottom")
  
 ggsave("DELNA vs EPT.png", width = 16, height = 10, dpi = 300)

# DIAG vs DELNA coded by EPT
devtools::source_gist("https://gist.github.com/kdauria/524eade46135f6348140")

ggplot(master, aes(x = DELNA_score, y = DIAG_score)) +
  geom_hline(aes(yintercept = 70), alpha = 0.5, color = "grey50", data = summary) +
  annotate("text", y = 70, x = 95,label = "Diagnostic Cutoff", vjust = 1.2) +
  geom_vline(aes(xintercept = 60), alpha = 0.5, color = "grey50", data = summary) +
  annotate("text", y = 100, x = 60,label = "DELNA Cutoff", hjust = -0.2) +
  geom_point(aes(fill = factor(EPT_score)), size = 8, pch = 21) +
  stat_smooth(method="lm", se=FALSE) +
  stat_smooth_func(geom="text", method="lm", hjust = 0, vjust = -1.5, parse = TRUE) +
  labs(x = "DELNA Score", y = "\nDiagnostic Score" ) +
  scale_fill_manual (values = c("green",wes_palette("Zissou")[5],wes_palette("Zissou")[3],wes_palette("Zissou")[1]), labels=c("F"="Fail", "MP"="Marginal Pass", "P"="Pass"), breaks = c("F","MP","P"), name = "EPT Score") +
  scale_x_continuous(limits=c(0,100)) +
  scale_y_continuous(limits=c(0,100)) +
  theme_tufte(base_size = 20) 

ggsave("DIAG vs DELNA vs EPT.png", width = 16, height = 10, dpi = 300)


# Diagnostic Plots
# Overall bar mean score by outcome %
diag_scores %>% 
  gather(item, score, -studentid) %>% 
  separate(item, c("test", "question", "outcome"), sep="_") %>% 
  filter(question!="Total", outcome != "Total") %>% 
  mutate(percent_score = score/3) %>% 
  group_by(outcome) %>% 
  summarize(mean = mean(percent_score)) %>% 
  ggplot(.,aes(x=outcome, y=mean)) +
  geom_bar(stat="identity", fill="grey50", width = 0.25) +
  geom_text(aes(label=percent(round(mean,3))), vjust = -1, hujst = 1) +
  geom_hline(yintercept = seq(0,1,.25), color = "white", lwd =1) +
  annotate("text", x = 3, y = .95, adj = 1,  family="serif", size = 8,
           label = str_wrap(c("Average diagnostic outcomes scores for 35 students"),30)) +
  scale_y_continuous(limits = c(0,1), labels = percent) +  
  theme_tufte(base_size = 30, ticks = FALSE) +
  theme(axis.title=element_blank())
 
ggsave("DIAG outcome scores.png", width = 16, height = 10, dpi = 300)

# outcome distributions
data<-diag_scores %>% 
  gather(item, score, -studentid) %>% 
  separate(item, c("test", "question", "outcome"), sep="_") %>% 
  filter(question!="Total", outcome != "Total") %>% 
  group_by(studentid,outcome) %>% 
  dplyr::summarize(o_score = plyr::round_any(median(score),.5)) %>% 
  group_by(outcome,o_score) %>% 
  tally  

  ggplot(data,aes(x=factor(o_score), y=n)) +
  geom_bar(stat="identity", fill = "grey50", width = 0.25, position = position_dodge()) +
  geom_hline(yintercept = seq(0,max(data$n),5), color = "white", lwd =1) +
  geom_text(aes(label = n), vjust = -1, size = 6) +
  geom_text(aes(x = 1.5, y = 25, label = outcome) , family="serif", size = 8, data = data %>% distinct()) +
  facet_wrap(~outcome) +
  scale_y_continuous(limits=c(0,30)) +
  theme_tufte(base_size = 30, ticks = FALSE) +
  theme(axis.title=element_blank(),
        panel.background = element_rect(fill=NA, color = "grey80"),
        strip.text = element_blank()) 

ggsave("DIAG outcome scores distribution.png",  width = 16, height = 10, dpi = 300)

# Parallel Coordinates plot

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

pc.data<-diag_scores %>% 
  gather(item, score, -studentid) %>% 
  separate(item, c("test", "question", "outcome"), sep="_") %>% 
  filter(question!="Total", outcome != "Total") %>% 
  group_by(studentid, outcome) %>% 
  summarize(mean = mean(score),
    r_mean = plyr::round_any(mean(score),1),
    mode = Mode(score)) %>% 
  ungroup

pc.color <- pc.data %>% 
  group_by(studentid) %>% 
  summarize(sum = sum(mean)) %>% 
  mutate(color = ifelse(sum< 10,"black","black")) %>% 
  ungroup

pc.number <- pc.data %>% 
  group_by(outcome, mode) %>% 
  tally %>% 
  ungroup

pc.data %<>%
  left_join(pc.color) 

# order <- pc.number %>%
#   arrange(desc(n)) %>%
#   select(outcome) %>% 
#   slice(1:7) %>% 
#   unlist
# 
# pc.data %<>% 
#   group_by(studentid) %>% 
#   mutate(outcome = factor(outcome,order)) 
#   

ggplot(pc.data, aes(x = outcome, y = mode)) +
  geom_path(aes(group = studentid, color = color), alpha = 0.2, position=position_jitter(w=0, h=0.02)) +
  stat_summary(fun.y = Mode, geom = "line", aes(group = 1), color = "blue", size = 2) +
  geom_point(data = pc.number, aes(size=n), pch=21, fill = "grey50") +
  theme_tufte(base_size = 14, ticks = FALSE) +
  scale_y_continuous(breaks = c(1,2,3)) +
  scale_color_identity() +
  scale_size_area(name = "Frequency" , max_size = 10) +
  theme(axis.title=element_blank(),
        strip.text = element_blank()) 



# GGally paracords

temp<-pc.data %>%
spread(outcome,mode, fill = 0) %>%
group_by(studentid) %>%
summarize_each(funs(sum), -c(1:5)) %>%
data.frame()

ggparcoord(temp, c(2:8), groupColumn = 1, scale = "globalminmax", order = "allClass")


# Correlations ----

delna_scores %>% 
  select(matches("DELNA_")) %>% 
  cor(.,use = "complete.obs") %>% 
  tidy -> temp

# Correlation function for dplyr::do with broom
diag.cor<-function(df) 
  {
 df %>% 
    unite(item,question,outcome) %>% 
    spread(item,score) %>% 
    select(contains("_")) %>% 
    as.matrix %>% 
    rcorr() %>% 
    tidy
}

# Dplyr & broom flow for item-total correlations
diag_scores %>% 
  gather(item, score, -studentid) %>% 
  separate(item, c("test", "question", "outcome"), sep="_") %>% 
  group_by(question, outcome) %>% 
  do(tidy(cor.test(.$age, .$circumference)))
  select(matches("DIAG_")) %>% 
  cor(., use = "complete.obs")





# Distribution DELNA Scores by country ----
world_map <- map_data("world")

delna_data<-delna_location %>% 
  rename(region = Country) %>% 
  left_join(world_map,.) %>% 
  group_by(region) %>% 
  summarise(`Average Score` = mean(Score))

p<-ggplot(delna_data, aes(map_id = region)) +
  geom_map(aes(fill = `Average Score`), map = world_map, color ="black") +
  expand_limits(x = world_map$long, y = world_map$lat) +
  xlab("") +
  ylab("") +
  scale_fill_gradientn(limits = c(0,100), colours=viridis(10), na.value = "white") +
  guides(fill = guide_colorbar(barwidth = 10, barheight = .5)) + 
  ggtitle("DELNA Distribution") +
  theme(
    text = element_text(family = "DejaVu Sans Condensed", size = 22),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 18),
    strip.background = element_blank(),
    strip.text.y = element_text(size = 18, angle = 0),
    strip.text.x = element_text(size = 18),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

ggsave("DELNA Chloropleth.pdf",p, width=11, height=8, dpi=300)
embed_fonts("DELNA Chloropleth.pdf")

library(googleVis)

# Make the map!
geoMap <- gvisGeoMap(delna_data, locationvar="region", numvar="Average Score",
                     options=list(dataMode="regions"))
  



  