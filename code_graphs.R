# title: "Code for Graphs for STI 2023 Poster"
# author: "Elena Chechik"
# org: "European University at St. Petersburg"


# Just run the code to get Figs for
# Uncovering Barriers for Women in Russian Grant Funding: Rejected and Supported Grant Applications

# Data will be downloaded from https://osf.io

  
# packages
library(tidyverse)
library(patchwork)

# data
grants_data <- read.csv(url("https://osf.io/qm2ez/download")) %>% 
  filter(area_name != "unknown") %>% 
  mutate(area_name = case_when(area_name == "IT" ~ "Information Technology",
                               TRUE ~ area_name))

# pallets
col_gender <- c("#ffe043","#b3b3b3")
col_gender2 <- c("#b3b3b3","#ffe043")

col_gap <- c("#91C45A", "#C19AD6")

col_8 <- c("#ADD8E6", "#8cb3d9", "#C19AD6", "#FFB6C1", 
           "#ffe043", "#91C45A", "#95E6D4", "#b3b3b3")

areas <- grants_data %>% 
  count(area_name) %>% arrange(desc(n)) %>% pull(area_name) 

myColors_8 <- setNames(col_8, areas)

# Figures

### Figure 1: Grants applications by research fields (1994-2016).

lvls <- grants_data %>% 
  group_by(area_name) %>% count() %>% ungroup() %>% 
  arrange(desc(n)) %>% pull(area_name)

p1 <- grants_data %>% 
  group_by(proj_year, area_name) %>% 
  count %>% ungroup() %>% 
  group_by(proj_year) %>% 
  mutate(sh = round(100 * n / sum(n), digits = 2)) %>% ungroup() %>% 
  mutate(area_name = factor(area_name, lvls)) %>% 
  ggplot() +
  geom_col(aes(x = proj_year, y = sh, fill = area_name), 
           color = "black", size = 0.1,
           width = 1, position = "fill", alpha = 0.8) + 
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(1995, 2000, 2005, 2010, 2015)) +
  labs(x = "Year") +
  scale_fill_manual(values = myColors_8, limits = force) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 8),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

p2 <- grants_data %>% 
  group_by(area_name) %>% count() %>% ungroup() %>% 
  mutate(area_name = factor(area_name, lvls)) %>% 
  ggplot() +
  geom_col(aes(x = reorder(area_name, n), y = n, fill = area_name), 
           color = "black", size = 0.1,
           width = 0.9, alpha = 0.8) +
  scale_fill_manual(values = myColors_8, limits = force) +
  coord_flip() +
  labs(x = "", y = "Grant Applications (1996-2016, count)") +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p2 + p1  

ggsave(filename = "figs/fig_1_sti23.png",
       family = "Helvetica",
       units = "mm", dpi = 300,  width = 184,
       height = 72)


### Figure 2: Proportion of women among grant applicants by field.

grants_data %>% 
  filter(gender != "unknown") %>% 
  group_by(proj_year, gender, area_name) %>% count %>% ungroup() %>% 
  group_by(proj_year,area_name) %>% 
  mutate(sh = round(100 * n / sum(n), digits = 2)) %>% ungroup() %>% 
  filter(gender == "female") %>% 
  
  ggplot(aes(x = proj_year, y = sh, group = area_name, fill = area_name)) +
  geom_col(color = "black", size = 0.1, alpha = 0.8) +
  geom_hline(yintercept = 50, linetype = 'dashed', size = 0.2, col = 'black') +
  facet_wrap(~ area_name, nrow = 2) + 
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50))+
  scale_x_continuous(breaks = c(1995, 2000, 2005, 2010, 2015)) +
  scale_fill_manual(values = myColors_8, limits = force) +
  labs(y = "Women among Applicants (%)") +
  
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 8),
        axis.text.x = element_text(size = 8, angle = 0, vjust = 0.5),
        axis.text.y = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = NA), 
        strip.text = element_text(colour = 'black', size = 8)) 

ggsave(filename = "figs/fig_2_sti23.png",
       family = "Helvetica",
       units = "mm", dpi = 300,  width = 184,
       height = 95)


### Figure 3: Proportion of supported applications by gender.

sgnf <-  data.frame(x = c(0.7, 1.7, 2.7, 3.7, 4.7, 5.7, 6.7, 7.7),
                    xend = c(1.3, 2.3, 3.3, 4.3, 5.3, 6.3, 7.3, 8.3),
                    y = rep(55, 8),
                    annot = c("***", "***", "**", "***", "***", "***", "***", "NS."))

lvls <- grants_data %>% 
  filter(gender != "unknown") %>% 
  filter(area_name != "NA") %>% 
  group_by(gender, area_name, statuse_project) %>% count() %>% ungroup() %>% 
  group_by(gender, area_name) %>% 
  mutate(sh = round(100 * n / sum(n), digits = 2)) %>% 
  filter(statuse_project == "accepted") %>% 
  filter(gender == "male") %>% arrange(desc(sh))

lvls <- as.character(lvls$area_name)

gap <- grants_data %>% 
  filter(gender != "unknown") %>% 
  filter(area_name != "NA") %>% 
  group_by(gender, area_name, statuse_project) %>% count() %>% ungroup() %>% 
  group_by(gender, area_name) %>% 
  mutate(sh = round(100 * n / sum(n), digits = 1)) %>% 
  filter(statuse_project == "accepted") %>% 
  select(gender, area_name, sh) %>% 
  pivot_wider(names_from = gender, values_from = sh) %>% 
  mutate(gap = male - female)

grants_data %>% 
  filter(gender != "unknown") %>% 
  filter(area_name != "NA") %>% 
  group_by(gender, area_name, statuse_project) %>% count() %>% ungroup() %>% 
  group_by(gender, area_name) %>% 
  mutate(sh = round(100 * n / sum(n), digits = 1)) %>% 
  filter(statuse_project == "accepted") %>% 
  mutate(area_name = factor(area_name, levels = lvls)) %>% 
  
  ggplot() +
  geom_col(aes(x = area_name, y = sh, fill = gender), 
           color = "black", size = 0.1,
           position = "dodge", width = 0.8,  
           alpha = 0.8)+
  geom_text(aes(x = area_name, y = sh + 2, group = gender, label = sh), 
            position = position_dodge(width = 0.8), size = 3) +
  geom_segment(data = sgnf,
               aes(x = x, xend = xend, y = y, yend = y), 
               size = 0.3) +
  geom_text(data = sgnf, 
            aes(label = annot, x = (xend + x) / 2, y = y + 2),  
            size = 3) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(breaks = c(0, 10, 20, 30 ,40, 50)) +
  scale_fill_manual(values = col_gender) +
  labs(fill = "",
       y = "Supported Applications (%)") + #,
  #caption = "\nPearson's Chi-squared test; NS. — not significant, * — p < 0.05, ** — p < 0.01, *** — p < 0.001") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "top",
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 8),
        plot.caption = element_text(size = 8, hjust = -0, face = "italic", color = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) 

ggsave(filename = "figs/fig_3_sti23.png",
       family = "Helvetica",
       units = "mm", dpi = 300,  width = 184,
       height = 84)

### Figure 4: Gender disparity in the proportion of supported applications by field.

grants_data %>% 
  filter(gender != "unknown") %>% 
  filter(area_name != "NA") %>% 
  group_by(proj_year, gender, area_name, statuse_project) %>% count() %>% ungroup() %>%
  group_by(proj_year, gender, area_name) %>% 
  mutate(sh = 100 * n / sum(n)) %>% 
  filter(statuse_project == "accepted") %>% 
  select(-n) %>% 
  pivot_wider(names_from = gender, values_from = sh) %>% 
  mutate(gap = round(male - female, digits = 2),
         color = case_when(gap > 0 ~ "gender gap in favor of men",
                           gap == 0 ~ "equal",
                           TRUE ~ "gender gap in favor of women")) %>% ungroup() %>% 
  
  ggplot() +
  geom_hline(yintercept = 0, linetype = 'solid', col = 'black', size = 0.2) +
  geom_col(aes(x = proj_year, y = gap, fill = color), color = "black", size = 0.1) + 
  geom_point(data = tibble(x = 2000, y = -20), aes(x, y), color = "white") +
  facet_wrap( ~ area_name, nrow = 2) +
  scale_fill_manual(values = col_gender2) +
  scale_x_continuous(breaks = c(1995, 2000, 2005, 2010, 2015)) +
  labs(y = "Gap (pp)", color = "Мore supported:") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "top",
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 8),
        axis.text.x = element_text(size = 8, angle = 0, vjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = NA), 
        strip.text = element_text(colour = 'black', size = 8))

ggsave(filename = "figs/fig_4_sti23.png",
       family = "Helvetica",
       units = "mm", dpi = 300,  width = 184,
       height = 110)

### Figure 6: Proportion of women among grant applicants by grant type (2005-2016).

four_first <- c("Biology & Medical Sciences", 
                "Chemistry & Material Sciences", 
                "Earth Sciences", "Engineering")

dat <- grants_data %>% 
  filter(proj_year >= 2005) %>% 
  filter(area_name != "unknown") %>% 
  filter(gender != "unknown") %>% 
  group_by(gender, area_name, status) %>% count %>% ungroup() %>% 
  group_by(area_name, status) %>% 
  mutate(sh = round(100 * n / sum(n), digits = 2)) %>% ungroup() %>% 
  mutate(status = factor(status, levels = c("not young no pubs/events", 
                                            "young no pubs/events", 
                                            "not young pubs/events", 
                                            "young pubs/events"))) %>% 
  filter(gender == "female") %>% select(-n, -gender) %>% 
  mutate(status2 = case_when(status == "not young no pubs/events" ~ "Non-Event/Pubs\nGoals",
                             status == "young no pubs/events" ~ "Non-Event/Pubs\nGoals",
                             status == "not young pubs/events" ~ "Event/Pubs\nGoals",
                             TRUE ~ "Event/Pubs\nGoals")) %>% 
  mutate(status3 = case_when(status == "not young no pubs/events" ~ "Non-Early\nCareer\nPI",
                             status == "young no pubs/events" ~ "Early\nCareer\nPI",
                             status == "not young pubs/events" ~ "Non-Early\nCareer\nPI",
                             TRUE ~ "Early\nCareer\nPI")) %>% select(-status)
p1 <- dat %>% 
  filter(area_name %in% four_first) %>% 
  ggplot(aes(x = status3, y = sh, fill = status3)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), 
           color = "black", size = 0.15, alpha = 0.8) +
  geom_hline(yintercept = 50, linetype = 'dashed', size = 0.2, col = 'black') +
  facet_grid(area_name ~ status2, labeller = label_wrap_gen(6)) + 
  scale_fill_manual(values = col_gap) +
  scale_y_continuous(breaks = c(0, 25, 50)) +
  labs(y = "Women among Applicants (%)") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 7, colour = 'black'),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        strip.background.y = element_blank(),
        strip.background.x = element_rect(fill = NA), 
        strip.text.y = element_text(colour = 'black', angle = 0, size = 8),
        strip.text.x = element_text(size = 8))


p2 <- dat %>% 
  filter(!(area_name %in% four_first)) %>% 
  ggplot(aes(x = status3, y = sh, fill = status3))+
  geom_bar(stat="identity", position = position_dodge(width = 0.5), 
           color = "black", size = 0.15, alpha = 0.85) +
  geom_hline(yintercept = 50, linetype = 'dashed', size = 0.2, col = 'black') +
  facet_grid(area_name ~ status2, labeller = label_wrap_gen(6)) + 
  scale_fill_manual(values = col_gap) +
  scale_y_continuous(breaks = c(0, 25, 50)) +
  labs(y = "Women among Applicants (%)") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 7, colour = 'black'),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        strip.background.y = element_blank(),
        strip.background.x = element_rect(fill = NA), 
        strip.text.y = element_text(colour = 'black', angle = 0, size = 8),
        strip.text.x = element_text(size = 8))
p1 + p2

ggsave(filename = "figs/fig_6_sti23.png",
       family = "Helvetica",
       units = "mm", dpi = 300,  width = 184,
       height = 120)

