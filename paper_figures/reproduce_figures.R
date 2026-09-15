################################################################################
# STRAVVS: A flexible agent-based modelling framework for simulating dynamic 
# value chain networks
# 
# Benjamin JJ McCormick,1* Nicholas Roxburgh,2 and J Gareth Polhill2
# 
# 1 The Rowett Institute, University of Aberdeen, Aberdeen, UK
# 2 Information & Computational Sciences, The James Hutton Institute, Aberdeen, UK
# 
# email: benjamin.mccormick@abdn.ac.uk
#
# Last updated: 11 Sept 2026
################################################################################

## Clear workspace
rm(list=ls())

## Load useful libraries
library(tidyverse)
library(ggplot2)
library(ggdist)
library(ggpubr)

## Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


## SYSTEM 1 --------------------------------------------------------------------
system1 <- read_csv("system1/STRAVVS system1-table.csv", skip = 6)

system1 <- system1 %>%
  # reshape to long-form
  pivot_longer(cols = starts_with("table:get")) %>%
  # tidy output names from BehaviorSpace
  mutate(name = gsub("table:get things-|\\\"","",name)) %>%
  rename(step = `[step]`,
         run = `[run number]`) %>%
  # split label into how the resource is moved and which resource
  separate(name, c("action", "resource"), sep =" ") %>%
  # replace resources that were not present
  mutate(value = as.numeric(gsub("<WrappedExtensionException>","0",value))) %>%
  # reset actions for a better plotting order
  mutate(action = factor(action, 
                         levels = c("ordered","imported","made",
                                    "delivered","consumed"))) %>%
  group_by(action, resource, step) %>%
  summarise(
    mean_value = mean(value, na.rm=T),
    median_value = quantile(value, 0.5, na.rm=T),
    low_value = quantile(value, 0.25, na.rm=T),
    high_value = quantile(value, 0.75, na.rm=T))

system1 %>%
  filter(str_detect(resource, "^Process", negate = T)) %>%
  ggplot(., aes(x = step, y = median_value)) +
  geom_ribbon(aes(ymin=low_value, ymax=high_value), alpha=0.3, fill = "gold")  + 
  geom_line(colour = "darkorange3", linewidth = 0.7) +
  facet_grid(resource ~ action) +
  labs(y = "Count of process calls",
       caption = "Line, mean; Area, interquartile range") +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = rel(1.1)))

system1 %>%
  ungroup() %>%
  filter(str_detect(resource, "^Process", negate = T)) %>%
  dplyr::select(resource, step, mean_value) %>%
  pivot_wider(names_from = resource, values_from = mean_value) %>%
  ggplot(., aes(x = Process1, y = Process2, colour = step)) +
  geom_point()

## SYSTEM 2 --------------------------------------------------------------------
system2 <- read_csv("system2/STRAVVS system2-table.csv", skip = 6)

system2 <- system2 %>%
  # reshape to long-form
  pivot_longer(cols = starts_with("table:get")) %>%
  # tidy output names from BehaviorSpace
  mutate(name = gsub("table:get things-|\\\"","",name)) %>%
  rename(step = `[step]`,
         run = `[run number]`) %>%
  # split label into how the resource is moved and which resource
  separate(name, c("action", "resource"), sep =" ") %>%
  # replace resources that were not present
  mutate(value = as.numeric(gsub("<WrappedExtensionException>","0",value))) %>%
  # reset actions for a better plotting order
  mutate(action = factor(action, 
                         levels = c("ordered","imported","made",
                                    "delivered","consumed"))) %>%
  group_by(action, resource, step) %>%
  summarise(
    mean_value = mean(value, na.rm=T),
    median_value = quantile(value, 0.5, na.rm=T),
    low_value = quantile(value, 0.25, na.rm=T),
    high_value = quantile(value, 0.75, na.rm=T))

# plot calls to each process
figure2 <- system2 %>%
  filter(str_detect(resource, "^Process", negate = F)) %>%
  ggplot(., aes(x = step, y = median_value)) +
  geom_ribbon(aes(ymin=low_value, ymax=high_value), alpha=0.3, fill = "gold")  + 
  geom_line(colour = "darkorange3", linewidth = 0.7) +
  facet_wrap(~resource, nrow = 1) +
  labs(y = "Count of process calls",
       caption = "Line, mean; Area, interquartile range") +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = rel(1.1)))
ggsave("figure2.png", figure2, height = 3, width = 7.5)

# plot count of every resource at each stage
system2 %>%
  filter(str_detect(resource, "^Process", negate = T)) %>%
  ggplot(., aes(x = step, y = median_value)) +
  geom_ribbon(aes(ymin=low_value, ymax=high_value), alpha=0.3, fill = "gold")  + 
  geom_line(colour = "darkorange3", linewidth = 0.7) +
  facet_grid(resource ~ action) +
  labs(y = "Count of process calls",
       caption = "Line, mean; Area, interquartile range") +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = rel(1.1)))
  


## SYSTEM 3 --------------------------------------------------------------------
system3 <- read_csv("system3/STRAVVS system3-table.csv", skip = 6)

system3 <- system3 %>%
  # reshape to long-form
  pivot_longer(cols = starts_with("table:get")) %>%
  # tidy output names from BehaviorSpace
  mutate(name = gsub("table:get things-|\\\"","",name)) %>%
  rename(step = `[step]`,
         run = `[run number]`) %>%
  # split label into how the resource is moved and which resource
  separate(name, c("action", "resource"), sep =" ") %>%
  # replace resources that were not present
  mutate(value = as.numeric(gsub("<WrappedExtensionException>","0",value))) %>%
  # reset actions for a better plotting order
  mutate(action = factor(action, 
                         levels = c("ordered","imported","made",
                                    "delivered","consumed"))) %>%
  mutate(resource = case_when(
    resource == "beef.calf" ~ "beef.calf (n)",
    resource == "sheep.lamb" ~ "sheep.lamb (n)",
    resource == "chicken.egg" ~ "chicken.egg (n 'mn)",
    resource == "beef.meat.kg" ~ "beef.meat (t)",
    resource == "sheep.meat.kg" ~ "sheep.meat (t)",
    resource == "milk.l" ~ "milk.(l '000)",
    resource == "wheat.grain.t" ~ "wheat.grain (t)",
    .default = resource
  )) %>%
  mutate(value = case_when(
    resource == "chicken.egg (n 'mn)" ~ value / 1000000,
    resource == "milk.(l '000)" ~ value / 1000,
    resource == "beef.meat (t)" ~ value / 1000,
    resource == "sheep.meat (t)" ~ value / 1000,
    .default = value
  )) %>%
  group_by(action, resource, step) %>%
  summarise(
    mean_value = mean(value, na.rm=T),
    median_value = quantile(value, 0.5, na.rm=T),
    low_value = quantile(value, 0.25, na.rm=T),
    high_value = quantile(value, 0.75, na.rm=T))
  
# plot count of every resource at each stage
figure3 <- system3 %>%
  filter(step >= 48) %>%
  filter(str_detect(resource, "^eq", negate = T)) %>%
  filter(action != "imported") %>%
  mutate(resource = gsub("\\.","\n", resource)) %>%
  ggplot(., aes(x = step, y = median_value)) +
  geom_ribbon(aes(ymin=low_value, ymax=high_value), alpha=0.3, fill = "gold")  + 
  geom_line(colour = "darkorange3", linewidth = 0.7) +
  facet_grid(resource ~ action, scales = "free_y") +
  scale_y_continuous(labels = scales::number_format(acuracy = 0)) +
  labs(y = "Count of resources",
       caption = "Line, mean; Area, interquartile range") +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = rel(0.8))) 
ggsave("figure4.png", figure3, height = 6, width = 7.5)


# plot calls to each process
system3 %>%
  filter(str_detect(resource, "^eq", negate = F)) %>%
  ggplot(., aes(x = step, y = median_value)) +
  geom_ribbon(aes(ymin=low_value, ymax=high_value), alpha=0.3, fill = "gold")  + 
  geom_line(colour = "darkorange3", linewidth = 0.7) +
  facet_wrap(~resource, nrow = 1, scales = "free_y") +
  labs(y = "Count of process calls",
       caption = "Line, mean; Area, interquartile range") +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = rel(1.1)))