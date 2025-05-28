# d&d.R
# Jamie Hudson
# Created: 28 May 2025
# Edited: 28 May 2025
# Data: Monster data from the Dungeons & Dragons System Reference Document

# load libraries ------------------------------------------------------------

library(showtext)
library(patchwork)
library(ggforce)
library(tidyverse)
font_add_google("MedievalSharp")
font_add_google("Caudex")
showtext_auto()

# load dataset ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2025, week = 21)

monsters <- tuesdata$monsters


# wrangle dataset ---------------------------------------------------------

monsters_names <- monsters %>% 
  slice_max(cr, n = 12) 

max_ability_score <- monsters_names %>% 
  select(str:cha) %>% 
  max(na.rm = TRUE)

name_order <- monsters_names %>% 
  arrange(desc(cr)) %>%  
  pull(name)

buffer = 3
teeth_gap = 0.5

offset <- -(max_ability_score + buffer)  # Add a bit of buffer space
label_buffer <- 1.7  # How far above the base to place labels

# First create the teeth_data 
teeth_data <- bind_rows(
  # Top triangles (saving throws, pointing down) - positioned in upper half
  monsters %>% 
    filter(name %in% monsters_names$name) %>% 
    select(name, str_save:cha_save) %>% 
    mutate(name = factor(name, levels = name_order)) %>%  
    pivot_longer(cols = str_save:cha_save, names_to = "variable", values_to = "score") %>% 
    mutate(
      variable = str_remove(variable, "_save"),
      x_num = as.numeric(factor(variable))
    ) %>% 
    group_by(name, variable) %>% 
    reframe(
      x = c(x_num - teeth_gap, x_num, x_num + teeth_gap, x_num - teeth_gap),
      y = c(0, -score, 0, 0),  
      variable = variable,
      direction = "top",
      score = score
    ),
  
  # Bottom triangles (ability scores, pointing up) - positioned in lower half  
  monsters %>% 
    filter(name %in% monsters_names$name) %>% 
    select(name, str:cha) %>% 
    mutate(name = factor(name, levels = name_order)) %>%  
    pivot_longer(cols = str:cha, names_to = "variable", values_to = "score") %>% 
    mutate(x_num = as.numeric(factor(variable))) %>% 
    group_by(name, variable) %>% 
    reframe(
      x = c(x_num - teeth_gap, x_num, x_num + teeth_gap, x_num - teeth_gap),
      y = c(offset, score + offset, offset, offset),  
      variable = variable,
      direction = "bottom",
      score = score
    )
)

# Create labels dataset
labels_data <- bind_rows(
  monsters %>% 
    filter(name %in% monsters_names$name) %>% 
    select(name, str_save:cha_save) %>% 
    mutate(name = factor(name, levels = name_order)) %>%  
    pivot_longer(cols = str_save:cha_save, names_to = "variable", values_to = "score") %>% 
    mutate(
      variable = str_remove(variable, "_save"),
      x_num = as.numeric(factor(variable)),
      y = 0 - label_buffer,  
      direction = "top"
    ),
  
  # Labels for bottom triangles (slightly above the offset base)
  monsters %>% 
    filter(name %in% monsters_names$name) %>% 
    select(name, str:cha) %>% 
    mutate(name = factor(name, levels = name_order)) %>%  
    pivot_longer(cols = str:cha, names_to = "variable", values_to = "score") %>% 
    mutate(
      x_num = as.numeric(factor(variable)),
      y = offset + label_buffer,  
      direction = "bottom"
    )
)


# plot --------------------------------------------------------------------

(plot <- teeth_data %>% 
  ggplot(aes(x = x, y = y, group = interaction(name, variable, direction))) +
    geom_rect(mapping=aes(xmin=0, xmax=7, ymin=0+5, ymax=-max_ability_score-buffer-5), 
              fill="#C76B5E",
              color="#C76B5E", alpha=0.5) +
  geom_ellipse(aes(x0 = 0.5, y0 = -(max_ability_score+buffer)/2, a = 0.25, b = (max_ability_score+buffer)/2, angle = 0), fill = "#8D282B",
               colour = "#8D282B") +
  geom_ellipse(aes(x0 = 6.5, y0 = -(max_ability_score+buffer)/2, a = 0.25, b = (max_ability_score+buffer)/2, angle = 0), fill = "#8D282B",
               colour = "#8D282B") +
  geom_rect(mapping=aes(xmin=0.5, xmax=6.5, ymin=0, ymax=-max_ability_score-buffer),
            fill="#8D282B",
            color="#8D282B", alpha=0.5) +
  geom_polygon(fill = "#CEC2AE", colour = "black") +
  geom_text(data = labels_data,
            aes(x = x_num, y = y, label = score, group = NULL),
            size = 20, colour = "black",
            family = "Caudex") +
  facet_wrap(~name, labeller = labeller(name = label_wrap_gen(15))) +
    lims(y = c(-45, 5)) +  
  scale_x_continuous(breaks = 1:6, labels = c("str", "dex", "con", "int", "wis", "cha")) +
  labs(x = "Ability", y = "Score") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black"),
    strip.text = element_text(colour = "#EEEEEE", size = 130, lineheight = 0.3),
    strip.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    plot.margin = margin(10, 10, 10, 10),
    text = element_text(family = "MedievalSharp")
  )
)

### legend

legend_offset <- -18

(teeth_legend <- tibble(
  variable = c("str", "dex", "con", "int", "wis", "cha"),
  x_num = 1:6,
  top_score = rep(7, 6),  # Example saving throw values
  bottom_score = rep(7, 6)  # Example ability scores
) %>% 
    bind_rows(
      select(., variable, x_num, score = top_score) %>% 
        group_by(variable) %>% 
        reframe(
          x = c(x_num - teeth_gap, x_num, x_num + teeth_gap, x_num - teeth_gap),
          y = c(0, -score, 0, 0),
          direction = "top"
        ),
      select(., variable, x_num, score = bottom_score) %>% 
        group_by(variable) %>% 
        reframe(
          x = c(x_num - teeth_gap, x_num, x_num + teeth_gap, x_num - teeth_gap),
          y = c(legend_offset, score + legend_offset, legend_offset, legend_offset),
          direction = "bottom"
        )
    ) %>% 
    ggplot(aes(x = x, y = y, group = interaction(variable, direction))) +
    geom_rect(mapping=aes(xmin=0, xmax=7, ymin=5, ymax=legend_offset - 5), 
              fill="#C76B5E",
              color="#C76B5E", alpha=0.5) +
    geom_ellipse(aes(x0 = 0.5, y0 = (legend_offset)/2, a = 0.25, b = (legend_offset)/2, angle = 0), fill = "#8D282B",
                 colour = "#8D282B") +
    geom_ellipse(aes(x0 = 6.5, y0 = (legend_offset)/2, a = 0.25, b = (legend_offset)/2, angle = 0), fill = "#8D282B",
                 colour = "#8D282B") +
    geom_rect(mapping=aes(xmin=0.5, xmax=6.5, ymin=0, ymax = legend_offset), 
              fill="#8D282B",
              color="#8D282B", alpha=0.5) +
    geom_polygon(fill = "#CEC2AE", colour = "black") +
    lims(y = c(-26, 9),
         x = c(-1, 8)) +  
    coord_cartesian(clip = "off") +
    geom_textbox(data = tibble(x_num = c(1, 3, 5),
                            label = c("Strength", "Constitution", "Wisdom")),
              aes(x = x_num, y = 2, label = label, group = NULL),
              halign = 0.5,
              width = grid::unit(0.73, "npc"), # 73% of plot panel width
              size = 25,
              family = "Caudex",
              colour = "#EEEEEE",
              box.colour = NA,
              lineheight = 0.3,
              fill = NA
              ) +
    geom_textbox(data = tibble(x_num = c(2, 4, 6),
                               label = c("Dexterity", "Intelligence", "Charisma")),
                 aes(x = x_num, y = -20.5, label = label, group = NULL),
                 halign = 0.5,
                 width = grid::unit(0.73, "npc"), 
                 size = 25,
                 family = "Caudex",
                 colour = "#EEEEEE",
                 box.colour = NA,
                 lineheight = 0.3,
                 fill = NA
    ) +
    annotate("text", x = 3.5, y = 9, label = "Size of Upper Teeth = Saving Throw Bonus", color = "#EEEEEE", 
             size = 30,
             family = "Caudex"
             ) +
    annotate("text", x = 3.5, y = -26,
             label = "Size of Lower Teeth = Ability Scores", color = "#EEEEEE", 
             size = 30,
             family = "Caudex"
             ) +
    theme_void() +
    theme(
      panel.background = element_rect(fill = "black"),
      plot.background = element_rect(fill = "black"),
      axis.title.y = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      plot.margin = margin(0, 10, 0, 10)
    )
)

subtitle <- "The top 12 monsters arranged by Challenge Rating (how challenging a monster is for a party of adventurers). Each set of teeth represent the scores for different attributes of individual monsters, which reflect the monster's raw physical and mental traits. The upper teeth show the corresponding saving throw bonuses, which indicate how resistant the monster is to certain effects in combat. These bonuses may include additional proficiency modifiers, depending on the monster's training or role." 

(final_plot <- plot +
  plot_annotation(title = "D&D Monsters",
                  subtitle = str_wrap(subtitle, 78),
                  caption = "@jamie_bio | source: System Reference Document v5.2.1",
                  theme = theme(plot.title = element_text(size = 275,
                                                          margin = margin(20,0,0,0),
                                                          family = "MedievalSharp",
                                                          colour = "#EEEEEE",
                                                          hjust = 0.15),
                                plot.subtitle = element_text(size = 95,
                                                             lineheight = 0.3,
                                                             margin = margin(40, 0, 50, 0),
                                                             family = "Caudex",
                                                             colour = "#EEEEEE"),
                                plot.caption = element_text(size = 50,
                                                            family = "MedievalSharp",
                                                            colour = "#EEEEEE",
                                                            margin = margin(0, 0, 20, 0)),
                                panel.background = element_rect(fill = "black"),
                                plot.background = element_rect(fill = "black",
                                                               colour = "black"),
                                plot.margin = margin(30,30,0,30))) +
  inset_element(teeth_legend, left = 0.625, bottom = 1.05, right = 1.025, top = 1.3, align_to = 'full', clip = T))

ggsave(paste0("d&d_monsters_", format(Sys.time(), "%d%m%Y"), ".png"), 
       dpi = 320,
       width = 24,
       height = 24)

