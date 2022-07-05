source(here::here("R", "utilities.R"))
load(here("output", "summ_list.Rda"))
load(here("output", "summ_list_pid.Rda"))
load(here("output/ANES/ANES_perf.Rda"))


# RESULT: ALIGNMENT
summ_df[["logit"]] %>% 
  tibble() %>%
  filter(`Variable Specification` %in%
           c("Demo. + Issues","Demo. + PID")) %>%
  rename(`Predictors` = `Variable Specification`) %>%
  ggplot(aes(x=Year, y=Accuracy, color = `Predictors` )) +
  geom_pointrange(aes(ymin=Accuracy_lower,ymax=Accuracy_upper),
                  position = position_dodge(width = .75), size=.6) +
  jcolors::scale_color_jcolors() +
  theme_bw() +
  geom_hline(yintercept = .5,lty=2) +
  theme(text = element_text(size=13)) +
  labs(title = "Accuracy of logit-based predictions of two-party presidential vote choice",
       y = "Percept of correctly predicted voting decisions") +
  scale_y_continuous(labels = scales::percent)
ggsave("fig/vote-choice-2specs_logit.pdf")

summ_df[["rf"]] %>% 
  tibble() %>%
  filter(`Variable Specification` %in%
           c("Demo. + Issues","Demo. + PID")) %>%
  rename(`Predictors` = `Variable Specification`) %>%
  ggplot(aes(x=Year, y=Accuracy, color = `Predictors` )) +
  geom_pointrange(aes(ymin=Accuracy_lower,ymax=Accuracy_upper),
                  position = position_dodge(width = .75), size=.6) +
  jcolors::scale_color_jcolors() +
  theme_bw() +
  geom_hline(yintercept = .5,lty=2) +
  theme(text = element_text(size=13)) +
  labs(title = "Accuracy of RF-based predictions of two-party presidential vote choice",
       y = "Percept of correctly predicted voting decisions") +
  scale_y_continuous(labels = scales::percent)
ggsave("fig/vote-choice-2specs_rf.pdf")


# RESULT: LOGIT OVERFITS
summ_df[["logit"]] %>% 
  tibble() %>%
  filter(`Variable Specification` %in%
           c("Demographics Only", "Demo. + PID", "All Covariates")) %>%
  ggplot(aes(x = Year, 
             y = `Variable Specification`, 
             fill = Accuracy, label = round(Accuracy,2))) +
  geom_tile(alpha=.87) +
  ggfittext::geom_fit_text(place = "topleft",size=11) +
  #scale_fill_steps(low = "white", high = "darkblue",breaks = c(20,30,60,70,80)) +
  #scale_fill_gradient(low = "white", high = "darkblue") +
  jcolors::scale_fill_jcolors_contin(palette = "pal12", reverse=TRUE) +
  #scale_fill_distiller(direction = 1) +
  labs(y = "Features included",
       subtitle = "Logit-based accuracy") +
  theme_minimal() +
  theme(text=element_text(size=14),
        legend.position = "top",
        axis.text.x = element_text(angle=90,vjust = 0.7, hjust=0, size=11))

summ_df[["rf"]] %>% 
  tibble() %>%
  filter(`Variable Specification` %in%
           c("Demographics Only", "Demo. + PID", "All Covariates")) %>%
  ggplot(aes(x = Year, 
             y = `Variable Specification`, 
             fill = Accuracy, label = round(Accuracy,2))) +
  geom_tile(alpha=.87) +
  ggfittext::geom_fit_text(place = "topleft",size=11) +
  #scale_fill_steps(low = "white", high = "darkblue",breaks = c(20,30,60,70,80)) +
  #scale_fill_gradient(low = "white", high = "darkblue") +
  jcolors::scale_fill_jcolors_contin(palette = "pal12", reverse=TRUE) +
  #scale_fill_distiller(direction = 1) +
  labs(y = "Features included",
       subtitle = "RF-based accuracy") +
  theme_minimal() +
  theme(text=element_text(size=14),
        legend.position = "top",
        axis.text.x = element_text(angle=90,vjust = 0.7, hjust=0, size=11))



RF <- summ_df[["rf"]] %>% as_tibble()

RF <- RF %>% group_by(`Variable Specification`) %>%
  mutate(avgAccuracy = mean(Accuracy))

# Basic plot with 2 specifications:
summ_df[["logit"]] %>% 
  tibble() %>%
  filter(`Variable Specification` %in%
           c("Demo. + Religion","Demographics Only")) %>%
  ggplot(aes(x=Year, y=Accuracy, color = `Variable Specification` )) +
  geom_pointrange(aes(ymin=Accuracy_lower,ymax=Accuracy_upper),
                  position = position_dodge(width = .75), size=.6) +
  #coord_flip() +
  jcolors::scale_color_jcolors()




summ_df[["logit"]] %>% 
  tibble() %>%
  filter(`Variable Specification` %in%
           c("Demo. + PID")) %>%
  rename(`Predictors` = `Variable Specification`) %>%
  ggplot(aes(x=Year, y=Accuracy, color = `Predictors` )) +
  geom_pointrange(aes(ymin=Accuracy_lower,ymax=Accuracy_upper),
                  position = position_dodge(width = .75), size=.6) +
  jcolors::scale_color_jcolors() +
  theme_bw() +
  geom_hline(yintercept = .5,lty=2) +
  theme(text = element_text(size=13)) +
  labs(title = "Accuracy of logit-based predictions of two-party presidential vote choice",
       y = "Percept of correctly predicted voting decisions") +
  scale_y_continuous(labels = scales::percent, limits = c(.5,1))






summ_df[["rf"]] %>%  count(`Variable Specification`)

WhichSpecs <- c("Demographics Only",
                "Demo. + PID",
                "All Covariates",
                "Demo. Only, Broader Definition",
                "Demo. + Issues")

# A heatmap with selected Spec:
RF %>%
  filter(`Variable Specification` %in% WhichSpecs) %>%
  ggplot(aes(x = Year, 
             y = fct_reorder(`Variable Specification`,-avgAccuracy), 
             fill = Accuracy, label = round(Accuracy,2))) +
  geom_tile(alpha=.87) +
  ggfittext::geom_fit_text(place = "topleft",size=11) +
  #scale_fill_steps(low = "white", high = "darkblue",breaks = c(20,30,60,70,80)) +
  #scale_fill_gradient(low = "white", high = "darkblue") +
  jcolors::scale_fill_jcolors_contin(palette = "pal2", reverse=TRUE) +
  #scale_fill_distiller(direction = 1) +
  labs(y = "Features included",
       subtitle = "RF-based accuracy") +
  theme_minimal() +
  theme(text=element_text(size=14),
        legend.position = "top",
        axis.text.x = element_text(angle=90,vjust = 0.7, hjust=0, size=11))
  
# A heatmap (with explicitly given numbers - ALL SPECs)
summ_df[["rf"]] %>% 
  tibble() %>%
  ggplot(aes(x = Year, 
             y = `Variable Specification`, 
             fill = Accuracy, label = round(Accuracy,2))) +
  geom_tile(alpha=.77) +
  ggfittext::geom_fit_text(place = "topleft",size=11) +
  #scale_fill_steps(low = "white", high = "darkblue",breaks = c(20,30,60,70,80)) +
  #scale_fill_gradient(low = "white", high = "darkblue") +
  jcolors::scale_fill_jcolors_contin(palette = "pal2", reverse=TRUE) +
  #scale_fill_distiller(direction = 1) +
  labs(y = "Features included",
       subtitle = "RF-based accuracy") +
  theme_minimal() +
  theme(text=element_text(size=14),
        legend.position = "top",
        axis.text.x = element_text(angle=90,vjust = 0.7, hjust=0, size=11))


