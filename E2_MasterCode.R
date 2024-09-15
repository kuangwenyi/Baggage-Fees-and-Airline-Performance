
##### Baggage Fees and Airline Performance #####


## Section 1 Data Preparation #####

# load package
# please install the relevant packages if not installed
pkgs = c(
  'tidyverse','patchwork','fastDummies','ggthemes','did','bacondecomp',
  'kableExtra','fixest','ggplot2','readxl','readr','tidyr',
  'dplyr','stringr','lme4','RColorBrewer','broom.mixed', 'TwoWayFEWeights', 
  'DIDmultiplegt', 'here')

kwy = lapply(pkgs, library, character.only=TRUE)
here()

# set blank plot theme for all Figures in the manuscript
theme_set(theme_clean() + theme(plot.background = element_blank(),
                                legend.background = element_blank()))

# read in data
Essay3_R <- read_csv(here("E1_csv.csv"))
colnames(Essay3_R)
unique(Essay3_R$airline)

# occasion list
test = Essay3_R %>% select(occasion, yearq)
test = test[!duplicated(test$yearq),]

# create bag implementation occasions (FirstTreat occasion)
Essay3_R$FirstTreat = ifelse(Essay3_R$airline %in% c("AMERICAN","UNITED", "US"),
                             18,
                             0)

Essay3_R$FirstTreat = ifelse(Essay3_R$airline%in% c("NORTHWEST","CONTINENTAL","FRONTIER"),
                             19,
                             Essay3_R$FirstTreat)

Essay3_R$FirstTreat = ifelse(Essay3_R$airline %in% c("DELTA","AIRTRAN"),
                             20,
                             Essay3_R$FirstTreat)

# create Treated Variable
Essay3_R$Treated = ifelse(Essay3_R$airline %in% c("AMERICAN","UNITED", "US")& 
                            Essay3_R$occasion > 18,
                          1,
                          0)

Essay3_R$Treated = ifelse(Essay3_R$airline %in% c("NORTHWEST","CONTINENTAL","FRONTIER") & 
                            Essay3_R$occasion > 19,
                          1,
                          Essay3_R$Treated)

Essay3_R$Treated = ifelse(Essay3_R$airline %in% c("DELTA","AIRTRAN") & 
                            Essay3_R$occasion > 20,
                          1,
                          Essay3_R$Treated)


# sanity check
unique(Essay3_R$FirstTreat)
unique(Essay3_R$Treated)

colnames(Essay3_R)

# Create necessary variables
Essay3_R$rel_occasion = Essay3_R$occasion - Essay3_R$FirstTreat
Essay3_R$FirstTreat_IE = ifelse(Essay3_R$FirstTreat == Essay3_R$occasion, 1, 0)
Essay3_R = Essay3_R %>% mutate(pOntime = ontime/totalrecords,
                               lop_revenues = log(op_revenues),
                               lpcpGDP = log(percpita_gdp),
                               lFare = log(avg_fare),
                               lCarrierDelay = log(aircarrierdelay), 
                               lpper100 = log(pper1000)
                               )

Essay3_R[is.na(Essay3_R)] = ""
Essay3_R[Essay3_R == -Inf]= ""
Essay3_R[Essay3_R == Inf]= ""

glimpse(Essay3_R)


##### Section 2 Model  #####
# TWFE estimates are biased. For comparison purpose only. 

##### 2.1 TWFE Event Study ------

# code adapted from Baker et al. 2022.
data_dummieBin <- Essay3_R  %>% 
  dummy_cols(select_columns = "rel_occasion", remove_selected_columns = FALSE,
             ignore_na = TRUE) %>% 
  mutate(across(starts_with("rel_occasion_"), ~replace_na(., 0))) %>%
  mutate(`rel_occasion_-6` = if_else(rel_occasion <= -6, -1, 0), 
         rel_occasion_6 = if_else(rel_occasion >= 6, 1, 0))

# create relative time indicators
indicatorBin <- c(paste0("`", "rel_occasion_", c(-6:-1, 1:6), "`"))
indicatorBinIE <- c(paste0("`", "rel_occasion_", c(-6:-2, 0:6), "`"))


###### 2.1.1 Complaint TWFE BIN ------

mod_TWFE_comp <- feols(lcomplaint ~ .[indicatorBin] +
                           lmisbagg + lCarrierDelay + 
                           lLFP + lfleetutil +
                           lrevpaxenplaned + lMKTshare + lempfte +
                           lhetero + lsparsity 
                           |carriercode, 
                           cluster = "carriercode", 
                           data = data_dummieBin)

summary(mod_TWFE_comp)

ES_TWFE_Comp

ES_TWFE_Comp <- broom::tidy(mod_TWFE_comp, conf.int = TRUE)[1:10,] %>%
  mutate(t = c(-4:-1, 1:6)) %>% 
  mutate(conf.low = conf.low - mean(estimate[t < 0]),
         conf.high = conf.high - mean(estimate[t < 0]),
         estimate = estimate - mean(estimate[t < 0])) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  bind_rows(tibble(t = 0, estimate = 0, conf.low = 0, conf.high = 0)) %>% 
  mutate(group = as.factor(case_when(
    t < 0 ~ 1,
    t >= 1 ~ 2
  ))) %>% 
  ggplot(aes(x = t, y = estimate)) + 
  geom_point(aes(fill= factor(group)),shape = 21) +
  scale_fill_manual(values = c("#993441", "#0029a5")) + geom_line() +
  ggtitle("The Dynamic Impact of Mergers on On-Time Performance - TWFE") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color=factor(group)), 
                linetype = "longdash", show.legend = FALSE) + 
  scale_color_manual("group", breaks=c(1,2),values=c("#993441", "#0029a5"))+
  geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
  geom_vline(xintercept = 0,  linetype = "longdash", color = "gray") + 
  labs(y = "Point Change", x = "Quarters Relative to Merger") + 
  scale_x_continuous(breaks = seq(-4, 6, by = 1)) + 
  scale_y_continuous(breaks = seq(-0.12, 0.1, by = 0.04)) + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 90),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")



######## 2.1.2 OTP TWFE BIN ######
mod_TWFE_OTP <- feols(lCarrierDelay ~ .[indicatorBin] +
                         lLFP + lfleetutil +
                         lrevpaxenplaned + lMKTshare + lempfte +
                         lhetero + lsparsity 
                       |carriercode + occasion, 
                       cluster = "occasion", 
                       data = data_dummieBin)

summary(mod_TWFE_OTP)

ES_TWFE_Comp

ES_TWFE_Comp <- broom::tidy(mod_TWFE_comp, conf.int = TRUE)[1:10,] %>%
  mutate(t = c(-4:-1, 1:6)) %>% 
  mutate(conf.low = conf.low - mean(estimate[t < 0]),
         conf.high = conf.high - mean(estimate[t < 0]),
         estimate = estimate - mean(estimate[t < 0])) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  bind_rows(tibble(t = 0, estimate = 0, conf.low = 0, conf.high = 0)) %>% 
  mutate(group = as.factor(case_when(
    t < 0 ~ 1,
    t >= 1 ~ 2
  ))) %>% 
  ggplot(aes(x = t, y = estimate)) + 
  geom_point(aes(fill= factor(group)),shape = 21) +
  scale_fill_manual(values = c("#993441", "#0029a5")) + geom_line() +
  ggtitle("The Dynamic Impact of Mergers on On-Time Performance - TWFE") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color=factor(group)), 
                linetype = "longdash", show.legend = FALSE) + 
  scale_color_manual("group", breaks=c(1,2),values=c("#993441", "#0029a5"))+
  geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
  geom_vline(xintercept = 0,  linetype = "longdash", color = "gray") + 
  labs(y = "Point Change", x = "Quarters Relative to Merger") + 
  scale_x_continuous(breaks = seq(-4, 6, by = 1)) + 
  scale_y_continuous(breaks = seq(-0.12, 0.1, by = 0.04)) + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 90),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")



##### 2.2 MODERATION EFFECT #####
# Static-Complaint
mod_static_comp <- feols(lcomplaint ~ Treated +
                        Treated*lmisbagg + 
                        lmisbagg + lCarrierDelay + 
                        lLFP + lfleetutil +
                        lrevpaxenplaned + lMKTshare + lempfte + 
                        lhetero + lsparsity
                        |carriercode + occasion, 
                        cluster = "occasion", 
                        data = stacked_data)

summary(mod_static_comp)


# OTP 
Essay3_R$lcc
mod_static_OTP <- feols(lCarrierDelay ~ Treated +
                     Treated*lLFP + 
                     lLFP + lfleetutil +
                     lrevpaxenplaned + lMKTshare + lempfte + 
                     lhetero + lsparsity + lcc
                    |carriercode, 
                    cluster = "occasion", 
                    data = data_dummieBin)

summary(mod_static_OTP)


# OPOR
mod_static_OPOR <- feols( OPOR ~ Treated +
                           Treated*lLFP + 
                           lFare + lpcpGDP +
                           lLFP + lfleetutil + 
                           lrevpaxenplaned + lMKTshare + lempfte +
                           lhetero + lsparsity
                         |carriercode + occasion, 
                         cluster = "occasion", 
                         data = stacked_data)

summary(mod_static_OPOR)


mod_static_Fare <- feols( OPOR ~ Treated +
                            Treated*lFare + 
                            lFare + lpcpGDP +
                            lLFP + lfleetutil + 
                            lrevpaxenplaned + lMKTshare + lempfte +
                            lhetero + lsparsity
                          |carriercode + occasion, 
                          cluster = "occasion", 
                          data = stacked_data)

summary(mod_static_Fare)


##### 2.3 Stacked Regression #####
# stacked regression result was reported 

# function to get treat-year specific cohorts for the event window
make_dt <- function(tyr) {
  Essay3_R %>% 
    #filter(OccasionDummy <= 52) %>% # drop observation after everyone is treated
    filter(FirstTreat == tyr | FirstTreat > tyr + 6) %>% 
    filter(occasion %>% between(tyr - 6, tyr + 6)) %>% # pre 6 post 6
    mutate(FirstTreat = if_else(FirstTreat == tyr, FirstTreat, NA_real_),
           rel_occasion = occasion - FirstTreat) %>% 
    select(carriercode,
           occasion, FirstTreat, Treated, 
           rel_occasion, 
           OPOR, pOntime, lop_revenues, #DV
           lLFP, lfp,lfleetutil,lhetero, lsparsity, # operational metrics
           lfueleff, lyield, lavglandfee, ltdomt_cost, # efficiency metrics
           lrevpaxenplaned, lempfte,# Human Metrics
           lmisbagg, ltotaldelay, lcomplaint, # Service quality metrics
           lpcpGDP, lFare, percent_gdp, percent_chg_fare, # Marco metrics
           lMKTshare, # market power metrics
           lcc, recession,
           lCarrierDelay, # carrier specific delay
           year, quarter) %>% 
    mutate(dt = as.character(tyr))
}


# treats
treats <- Essay3_R %>% 
  filter(FirstTreat <= max(FirstTreat)) %>% 
  pull(FirstTreat) %>% 
  unique() %>% 
  sort()

# stack the data 
stacked_data <- map_dfr(treats, make_dt) %>% 
  dummy_cols(select_columns = "rel_occasion", 
             remove_selected_columns = FALSE,
             ignore_na = TRUE) %>% 
  mutate(across(starts_with("rel_occasion_"), ~replace_na(., 0))) %>% 
  mutate(cluster = paste0(carriercode, "_", dt))

# make formula
indicatorStacked <- c(paste0("`", "rel_occasion_", c(-6:-1, 1:6), "`"))
indicatorIE <- c(paste0("`", "rel_occasion_", c(-6:-2, 0:6), "`"))

###### 2.3.1 Complaint Stacked ######
# Complaint Stacked
stack_comp_IE <- feols(lcomplaint ~ .[indicatorIE] + 
                       lmisbagg + lCarrierDelay + 
                       lLFP + lfleetutil +
                       lrevpaxenplaned + lMKTshare + lempfte + 
                       lhetero + lsparsity
                       |carriercode^dt + occasion,
                         cluster = "carriercode",
                         data = stacked_data)


summary(stack_comp_IE)


ES_COMP
ES_COMP <- broom::tidy(stack_comp_IE, conf.int = TRUE)[1:12,] %>%
  mutate(t = c(-6:-2, 0:6)) %>% 
  mutate(conf.low = conf.low - mean(estimate[t < 0]),
         conf.high = conf.high - mean(estimate[t < 0]),
         estimate = estimate - mean(estimate[t < 0])) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  mutate(group = as.factor(case_when(
    t < 0 ~ 1,
    t >= 1 ~ 2
  ))) %>% 
  ggplot(aes(x = t, y = estimate)) + 
  geom_point(aes(fill= factor(group)),shape = 21) +
  scale_fill_manual(values = c("#993441", "#0029a5")) + geom_line() +
  ggtitle("Impact of Baggage Fee Policy on Consumer Complaint") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color=factor(group)), 
                linetype = "longdash", show.legend = FALSE) + 
  scale_color_manual("group", breaks=c(1,2),values=c("#993441", "#0029a5"))+
  geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
  geom_vline(xintercept = 0,  linetype = "longdash", color = "gray") + 
  labs(y = "Point Change", x = "Quarters Relative to Baggage Fee Policy Implementation ") + 
  scale_x_continuous(breaks = seq(-6, 6, by = 1)) + 
  scale_y_continuous(breaks = seq(-2, 2, by = 0.5)) + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 90),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")


ggsave(ES_COMP, filename = here::here("Figs_Tables", "ES_COMP.png"), 
       dpi = 500, width = 6, height = 4)

###### 2.3.2 OPOR Stacked #######
# Immediate Effect
stack_OPOR_IE <- feols(OPOR ~ .[indicatorIE] + 
                         lFare + lpcpGDP +
                         lLFP + lfleetutil + lyield + 
                         lrevpaxenplaned + lMKTshare + lempfte +
                         lhetero + lsparsity
                       |carriercode^dt,
                       cluster = "occasion",
                       data = stacked_data)

summary(stack_OPOR_IE)

ES_OPOR
ES_OPOR <- broom::tidy(stack_OPOR_IE, conf.int = TRUE)[1:12,] %>%
  mutate(t = c(-6:-2, 0:6)) %>% 
  mutate(conf.low = conf.low - mean(estimate[t < 0]),
         conf.high = conf.high - mean(estimate[t < 0]),
         estimate = estimate - mean(estimate[t < 0])) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  mutate(group = as.factor(case_when(
    t <= 0 ~ 1,
    t >= 0 ~ 2
  ))) %>% 
  ggplot(aes(x = t, y = estimate)) + 
  geom_point(aes(fill= factor(group)),shape = 21) +
  scale_fill_manual(values = c("#993441", "#0029a5")) + geom_line() +
  ggtitle("Impact of Baggage Fee Policy on OPOR") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color=factor(group)), 
                linetype = "longdash", show.legend = FALSE) + 
  scale_color_manual("group", breaks=c(1,2),values=c("#993441", "#0029a5"))+
  geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
  geom_vline(xintercept = 0,  linetype = "longdash", color = "gray") + 
  labs(y = "Point Change", x = "Quarters Relative to Baggage Fee Policy Implementation ") + 
  scale_x_continuous(breaks = seq(-6, 6, by = 1)) + 
  scale_y_continuous(breaks = seq(-1, 1, by = 0.1)) + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 90),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")


ggsave(ES_OPOR, filename = here::here("Figs_Tables", "ES_OPOR.png"), 
       dpi = 500, width = 6, height = 4)


###### 2.3.3 OTP Stacked #######
stack_OTP_IE <- feols(lCarrierDelay ~ .[indicatorIE] + 
                     lLFP + lfleetutil + lyield +
                     lrevpaxenplaned + lMKTshare + lempfte + 
                     lhetero + lsparsity 
                   |carriercode^dt,
                   cluster = "carriercode",
                   data = stacked_data)

summary(stack_OTP_IE)

ES_OTP
ES_OTP <- broom::tidy(stack_OTP_IE, conf.int = TRUE)[1:12,] %>%
  mutate(t = c(-6:-2, 0:6)) %>% 
  mutate(conf.low = conf.low - mean(estimate[t < 0]),
         conf.high = conf.high - mean(estimate[t < 0]),
         estimate = estimate - mean(estimate[t < 0])) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  mutate(group = as.factor(case_when(
    t < 0 ~ 1,
    t >= 0 ~ 2
  ))) %>% 
  ggplot(aes(x = t, y = estimate)) + 
  geom_point(aes(fill= factor(group)),shape = 21) +
  scale_fill_manual(values = c("#993441", "#0029a5")) + geom_line() +
  ggtitle("Impact of Baggage Fee Policy on Carrier-Induced Delays") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color=factor(group)), 
                linetype = "longdash", show.legend = FALSE) + 
  scale_color_manual("group", breaks=c(1,2),values=c("#993441", "#0029a5"))+
  geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
  geom_vline(xintercept = 0,  linetype = "longdash", color = "gray") + 
  labs(y = "Point Change", x = "Quarters Relative to Baggage Fee Policy Implementation ") + 
  scale_x_continuous(breaks = seq(-6, 6, by = 1)) + 
  scale_y_continuous(breaks = seq(-1, 1, by = 0.2)) + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 90),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")


ggsave(ES_OTP, filename = here::here("Figs_Tables", "ES_OTP.png"), 
       dpi = 500, width = 6, height = 4)



###### 2.3.4 Coef - Demeaned #######
# Following Wooldridge (2010), demeaned results were reported

coef_comp = broom::tidy(stack_comp_IE, conf.int = TRUE)[1:12,] %>%
  mutate(t = c(-6:-2, 0:6)) %>% 
  mutate(conf.low = conf.low - mean(estimate[t < 0]),
         conf.high = conf.high - mean(estimate[t < 0]),
         estimate = estimate - mean(estimate[t < 0])) %>% 
  select(t, estimate, std.error, conf.low, conf.high) %>%
  mutate(tstats = estimate/std.error)


coef_OPOR = broom::tidy(stack_OPOR_IE, conf.int = TRUE)[1:12,] %>%
  mutate(t = c(-6:-2, 0:6)) %>% 
  mutate(conf.low = conf.low - mean(estimate[t < 0]),
         conf.high = conf.high - mean(estimate[t < 0]),
         estimate = estimate - mean(estimate[t < 0])) %>% 
  select(t, estimate, std.error, conf.low, conf.high) %>%
  mutate(tstats = estimate/std.error)


coef_OTP = broom::tidy(stack_OTP_IE, conf.int = TRUE)[1:12,] %>%
  mutate(t = c(-6:-2, 0:6)) %>% 
  mutate(conf.low = conf.low - mean(estimate[t < 0]),
         conf.high = conf.high - mean(estimate[t < 0]),
         estimate = estimate - mean(estimate[t < 0])) %>% 
  select(t, estimate, std.error, conf.low, conf.high) %>%
  mutate(tstats = estimate/std.error)

write.csv(coef_comp, file = "coef_comp.csv")
write.csv(coef_OPOR, file = "coef_OPOR.csv")
write.csv(coef_OTP, file = "coef_OTP.csv")


#### Section 3 Model Free Plots ####

###### 3.1 Individual Level ----------

# create bag implementation occasions (FirstTreat occasion)
plotdata1 = Essay3_R %>% filter(airline %in% c("AMERICAN","UNITED", "US",
  "NORTHWEST","CONTINENTAL","FRONTIER", "DELTA","AIRTRAN")) %>%
   select(airline, carriercode,yearq, occasion, OPOR, totalcomplaint, 
          aircarrierdelay, FirstTreat, rel_occasion) %>%
  filter(rel_occasion %in% c(-6, -5, -4, -3, -2, -1, 0, 1, 2,3,4,5,6)) 


# delay plot
plot_delay = plotdata %>% filter(airline %in% c("AMERICAN","UNITED")) %>%
ggplot(aes(rel_occasion,aircarrierdelay, group = airline, color = airline)) + 
  geom_point() + 
  geom_line() + 
  ggtitle("Carrier-induced Delay") +
  geom_vline(xintercept = 0, 
             color = "blue", size=0.8) +
   xlab("Occasion Relative to Baggage Fee Implementation") +
   ylab("Minutes of Carrier-induced Delays")

ggsave(plot_delay, filename = here::here("Figs_Tables", "Plot_Delay.png"), 
       dpi = 500, width = 6, height = 4)

# OPOR plot
plot_OPOR

# "AMERICAN","UNITED", "US",
# "NORTHWEST","CONTINENTAL","FRONTIER", "DELTA","AIRTRAN 
plot_OPOR = plotdata1 %>% filter(airline %in% c("DELTA")) %>%
  ggplot(aes(rel_occasion, OPOR)) + 
  geom_point() + 
  geom_line() + 
  ggtitle("OPOR") +
  geom_vline(xintercept = 0, 
             color = "blue", size=0.8) +
  xlab("Occasion Relative to Baggage Fee Implementation") +
  ylab("OPOR")

ggsave(plot_OPOR, filename = here::here("Figs_Tables", "Plot_OPOR_avg.png"), 
       dpi = 500, width = 6, height = 4)

# complaint plot
# "AMERICAN","UNITED", "US",
# "NORTHWEST","CONTINENTAL","FRONTIER", "DELTA","AIRTRAN 
plot_comp  = 
  plotdata1 %>% filter(airline %in% c("FRONTIER")) %>%
  ggplot(aes(rel_occasion, totalcomplaint)) + 
  geom_point() + 
  geom_line() + 
  ggtitle("Consumer Complaint") +
  geom_vline(xintercept = 0, 
             color = "blue", size=0.8) +
  xlab("Occasion Relative to Baggage Fee Implementation") +
  ylab("Number of Consumer Complaints")


ggsave(plot_comp, filename = here::here("Figs_Tables", "Plot_Comp_avg.png"), 
       dpi = 500, width = 6, height = 4)

# all test scale not okay
plotdata
ggplot(plotdata,aes(rel_occasion)) + 
  geom_line(aes(y = OPOR_avg), color = "darkred") + 
  geom_line(aes(y = Comp_avg), color="steelblue", linetype="twodash") +
  geom_line(aes(y = Delay_avg), color="steelblue", linetype="twodash") 

###### 3.2 Individual Plot ----------

plotdata2 = Essay3_R %>% filter(airline %in% 
                                  c("US",
                                               "NORTHWEST","CONTINENTAL","DELTA")) %>%
  select(airline, carriercode,yearq, occasion, OPOR, totalcomplaint, 
         aircarrierdelay, FirstTreat, rel_occasion) %>%
  filter(rel_occasion %in% c(-6, -5, -4, -3, -2, -1, 0, 1, 2,3,4,5,6)) %>%
  group_by(rel_occasion) %>%
  summarise(OPOR_avg = mean(OPOR), 
            Comp_avg = mean(totalcomplaint), 
            Delay_avg = mean(aircarrierdelay)) 

# Delay Data
plot_delay
plot_delay = plotdata2 %>% 
  ggplot(aes(rel_occasion,Delay_avg)) + 
  geom_point() + 
  geom_line() + 
  ggtitle("Carrier-induced Delay") +
  geom_vline(xintercept = 0, 
             color = "blue", size=0.8) +
  xlab("Occasion Relative to Baggage Fee Implementation") +
  ylab("Minutes of Carrier-induced Delays")

ggsave(plot_delay, filename = here::here("Figs_Tables", "Plot_Delay_avg.png"), 
       dpi = 500, width = 6, height = 4)

# OPOR PLOT
plot_OPOR_Avg
plotdata2 %>%
  ggplot(aes(rel_occasion, OPOR_avg)) + 
  geom_point() + 
  geom_line() + 
  ggtitle("OPOR") +
  geom_vline(xintercept = 0, 
             color = "blue", size=0.8) +
  xlab("Occasion Relative to Baggage Fee Implementation") +
  ylab("OPOR")


ggsave(plot_OPOR, filename = here::here("Figs_Tables", "Plot_OPOR.png"), 
       dpi = 500, width = 6, height = 4)

# complaint plot
plot_comp
plotdata2 %>%
  ggplot(aes(rel_occasion, Comp_avg)) + 
  geom_point() + 
  geom_line() + 
  ggtitle("Consumer Complaint") +
  geom_vline(xintercept = 0, 
             color = "blue", size=0.8) +
  xlab("Occasion Relative to Baggage Fee Implementation") +
  ylab("Number of Consumer Complaints")


ggsave(plot_comp, filename = here::here("Figs_Tables", "Plot_Comp.png"), 
       dpi = 500, width = 6, height = 4)


#### Section 4 correlation matrix --------------
data_cor = Essay3_R %>% select(OPOR, lCarrierDelay, lcomplaint, 
                              lLFP, lyield, lfleetutil, 
                              lhetero, lsparsity, 
                              lrevpaxenplaned, lempfte, 
                              lMKTshare, lmisbagg,
                              lFare, percent_gdp)
colnames(Essay3_R)

cor_result = cor(data_cor, use = "complete.obs")
write.csv(cor_result, file = "cor_result.csv")

columnmeans = as.data.frame(colMeans(data_cor, na.rm = T))
as.data.frame(apply(data_cor, 2, sd, na.rm=T))

as.data.frame(colSums(!is.na(data_cor)))











