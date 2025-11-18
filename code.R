## Figures made on  R version 4.3.2 ##


#### Install packages ####
# Skip this if you already have installed them
install.packages(c("tidyverse", "rworldmap", "sf", "ggtext",
                   "patchwork", "ggsci", "ggrepel", "ggpp",
                   "janitor"))

#### Load packages ####
library(tidyverse)
library(rworldmap)
library(sf)
library(ggtext)
library(patchwork)
library(ggsci)
library(ggrepel)
library(ggpp)
library(janitor)


#### Figure 1. world map ####
damage_factor <- read_csv("damage_factors_per_100000.csv", locale = locale(encoding = "latin1"))

damage_factor %>% glimpse()
damage_factor %>% 
    mutate(daly_cold_degree_year_capita = `Average Damage factor [DALY.C-1.yr-1], low temperature, per 100 000´` / 100000,
           daly_heat_degree_year_capita = `Average Damage factor [DALY.C-1.yr-1], high temperature, per 100 000´` / 100000,
           daly_heat_wadapt_degree_year_capita = `Average Damage factor [DALY.C-1.yr-1], high temperature with adaptation, per 100 000´` / 100000,
           daly_net_degree_year_capita = `Average Damage factor [DALY.C-1.yr-1], NET, per 100 000´` / 100000,
           daly_net_wadapt_degree_year_capita = `Average Damage factor [DALY.C-1.yr-1], NET with adaptation, per 100 000´` / 100000) %>% 
    filter(!is.na(ISO3)) -> damage_factor

damage_factor$ISO3[damage_factor$Country == "Taiwan"] <- "TWN"

# Load the world map and convert it to an sf object
world_map <- st_as_sf(getMap(resolution = "low"))
# Merge the data with the world map
map_data <- left_join(world_map, damage_factor, by = c("ISO_A3" = "ISO3")) %>% as_tibble()
# Select relevant columns to retain geometry and metric columns for melting
map_data_selected <- 
    map_data %>%
    select(geometry, ISO_A3, daly_heat_degree_year_capita:daly_net_wadapt_degree_year_capita)

# Reshape data for facet plotting, excluding cold metric
map_data_long <- 
    map_data_selected %>% 
    gather(key = "Metric", value = "Value", daly_heat_degree_year_capita:daly_net_wadapt_degree_year_capita) %>% 
    mutate(Metric = as.factor(Metric))


# Clean up metric names for better readability in the plot
map_data_long$Metric <- recode(map_data_long$Metric,
                               "daly_heat_degree_year_capita" = "Heat",
                               "daly_heat_wadapt_degree_year_capita" = "Heat (Adaptation)",
                               "daly_net_degree_year_capita" = "Net",
                               "daly_net_wadapt_degree_year_capita" = "Net (Adaptation)")

data_range_abs_max <- max(abs(range(map_data_long$Value, na.rm = TRUE)))
metric_levels <- map_data_long$Metric %>% levels()
panel_tags <- LETTERS[1:length(metric_levels)]
names(panel_tags) <- metric_levels
facet_labeller <- labeller(Metric = function(x) paste0(panel_tags[x], ": ", x))

# Ensure the Metric factor is in the correct order for plotting
map_data_long$Metric <- factor(map_data_long$Metric, 
                               levels = c("Heat", "Heat (Adaptation)", "Net", "Net (Adaptation)"))

# round up to certain digit
round_up_dec <- function(x, digits) {
    # The 10^digits is calculated and stored for efficiency
    power_of_10 <- 10^digits
    
    # Scale up, apply ceiling, then scale down
    result <- ceiling(x * power_of_10) / power_of_10
    
    return(result)
}

data_range_max <- round_up_dec(data_range_abs_max, 2)
data_range_min <- -data_range_max

# Create a list of plots, one for each metric
plot_list <- lapply(levels(map_data_long$Metric), function(metric_name) {
    ggplot(subset(map_data_long, Metric == metric_name)) +
        geom_sf(aes(fill = Value, geometry = geometry)) +
        scale_fill_steps2(
            low = "blue", mid = "white", high = "red",
            midpoint = 0,
            n.breaks = 12,
            nice.breaks = T,
            labels = c(-0.01, "", 0, "", 0.01, "", 0.02, "", 0.03, "", 0.04, "", 0.05, ""),
            na.value = "grey50",
            name = "**Per-capita Damage Factor**<br>(**DALY·°C<sup>-1</sup>·yr<sup>-1</sup>·cap<sup>-1</sup>**)",
            limits = c(min(map_data_long$Value, na.rm = TRUE), data_range_max)
            
        ) +
        coord_sf(ylim = c(-60, 90), expand = FALSE) +
        labs(title = metric_name) + 
        theme_minimal() +
        theme(
            plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
            legend.position = "bottom",
            legend.title = element_markdown(size = 18, vjust = 0.9, lineheight = 0.9, halign = 0.5),
            legend.text = element_text(size = 18),
            panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")
        )
})

# Combine the plots using patchwork
figure1_country_tagged <- 
    wrap_plots(plot_list, ncol = 2) + 
    plot_annotation(tag_levels = "A") + 
    plot_layout(guides = 'collect') &
    guides(
        fill = guide_colourbar(
            theme = theme(
                legend.key.width = unit(20, "cm")
            ))
    ) &
    theme(
        legend.position = 'bottom',
        plot.tag = element_text(size = 22, face = 'bold')
    )


ggsave(filename = "main_Figure1.png", figure1_country_tagged, width = 15, height = 10)



#### Figure 2 (scatter plot with boxplot insets) ####
figure2_data <- read.csv("income_graph_figure.csv") %>% as_tibble()
figure_country_names <- read.csv("figure_country_names.csv") %>% as_tibble()
figure_country_names %>% 
    mutate(ISO3 = gsub(" ", "", ISO3),
           country = ifelse(ISO3 == "SWZ", "Eswatini", country)) -> figure_country_names


figure2_data %>% 
    select(X:X.13) %>% 
    janitor::row_to_names(2) %>%
    rename(region = 1,
           GDP_per_capita = `GDP Per_Capita_PPP`,
           GDP_per_capita_future = `Future GDP Per_Capita (SSP3)`,
           scenario = `Climate Scenario`) %>%
    mutate(across(c(GDP_per_capita:`Country ID`, `Projected Temperature Increase`:`Excess Mortality per degree C/ DALY /100000Net Mortality Change With Income Adjustment for Heat`), as.numeric)) %>% 
    mutate(region = factor(region),
           scenario = factor(scenario, levels = c("RCP 2.6", "RCP 4.5", "RCP 6.0", "RCP 8.5"))) %>% 
    rename(heat_wo_adapt = `Excess Mortality per degree C/ DALY /100000Heat-Related Mortality (Heat Model 4) Without Income Adjustment for Heat`,
           heat_w_adapt = `Excess Mortality per degree C/ DALY/100000Related Mortality (Heat Model 4) With Income Adjustment for Heat`) %>% 
    select(ISO3, region, scenario, GDP_per_capita, GDP_per_capita_future, heat_wo_adapt, heat_w_adapt) %>% 
    left_join(figure_country_names %>% select(ISO3, country), by = c("ISO3" = "ISO3")) %>% 
    gather(key = adaptation, value = DALY_100000, heat_wo_adapt, heat_w_adapt) %>%
    mutate(adaptation = factor(adaptation, levels = c("heat_wo_adapt", "heat_w_adapt"),
                               labels = c("Heat without adaptation", "Heat with adaptation")),
           DALY_capita = DALY_100000 / 100000) -> figure2_data_clean



colors_order <- pal_aaas("default")(7)

colors_order <- c(
    "Sub-Saharan Africa" = colors_order[2],
    "South Asia" = colors_order[6],
    "Latin America & Caribbean" = colors_order[3],
    "East Asia & Pacific" = colors_order[4],
    "Middle East & North Africa" = colors_order[5],
    "Europe & Central Asia" = colors_order[1],
    "North America" = colors_order[7]
)

reds <- c(0, 112, 196, 153)
greens <- c(52, 160, 121, 0)
blues <- c(102, 205, 0, 2)
color_RCP <- rgb(reds, greens, blues, maxColorValue = 255)

figure2_data_clean %>% 
    filter(adaptation == "Heat without adaptation") %>% 
    ggplot(aes(x = scenario, y = DALY_capita)) +
    stat_boxplot(aes(color = scenario), size = 1) +
    labs(y = "**Per-capita Damage Factor**<br>(**DALY·°C<sup>-1</sup>·yr<sup>-1</sup>·cap<sup>-1</sup>**)", x = "RCP scenario") +
    scale_y_continuous(breaks = seq(-0.1, 0.1, 0.005), limits = c(-0.006, 0.026)) +
    theme_bw() +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13),
          axis.title.x = element_text(size = 15, face = "bold"),
          axis.title.y = element_markdown(size = 15, face = "bold", lineheight = 1.1, halign = 0.5)
    ) +
    scale_color_manual(values = c(
        "RCP 2.6" = color_RCP[1],
        "RCP 4.5" = color_RCP[2],
        "RCP 6.0" = color_RCP[3],
        "RCP 8.5" = color_RCP[4]
    )) -> p_woadaptation_boxplot


figure2_data_clean %>% 
    filter(adaptation == "Heat without adaptation" & scenario == "RCP 2.6") %>% 
    ggplot(aes(x = GDP_per_capita, y = DALY_capita)) +
    geom_point(aes(color = region), size = 4) +
    geom_text_repel(
        data = figure2_data_clean %>% 
            filter(adaptation == "Heat without adaptation" & scenario == "RCP 2.6"),
        aes(label = country), max.overlaps = 10, seed = 1, size = 6) + 
    geom_hline(aes(yintercept = mean(DALY_capita)), color = "darkgrey", linewidth = 1.3) + 
    geom_vline(aes(xintercept = mean(GDP_per_capita)), color = "darkgrey", linewidth = 1.3) + 
    scale_shape_manual(values = 15:18) +
    theme_bw() + 
    guides(color = guide_legend(title = "Region")) + 
    scale_x_continuous(breaks = seq(0, 140000, 10000), limits = c(0, 132000),
                       labels = scales::label_number(suffix = "k", scale = 1e-3)) +
    scale_y_continuous(breaks = seq(-0.1, 0.1, 0.005), limits = c(-0.006, 0.026)) +
    scale_color_manual(values = colors_order) + 
    xlab(expression(bold("Per-capita GDP" ~"(USD)"))) + 
    ylab("**Per-capita Damage Factor**<br>(**DALY·°C<sup>-1</sup>·yr<sup>-1</sup>·cap<sup>-1</sup>**)") +
    ggtitle("Heat without adaptation") + 
    theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
    annotate(geom = "plot",
             x = I(1),
             y = I(1),
             label = p_woadaptation_boxplot,
             vp.width = 0.6,
             vp.height = 0.6) -> figure2_heat_wo_adaptation


figure2_data_clean %>% 
    filter(adaptation == "Heat with adaptation") %>% 
    ggplot(aes(x = scenario, y = DALY_capita)) +
    stat_boxplot(aes(color = scenario), size = 1) +
    labs(y = "**Per-capita Damage Factor**<br>(**DALY·°C<sup>-1</sup>·yr<sup>-1</sup>·cap<sup>-1</sup>**)", x = "RCP scenario") +
    scale_y_continuous(breaks = seq(-0.1, 0.1, 0.005), limits = c(-0.006, 0.026)) +
    theme_bw() +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13),
          axis.title.x = element_text(size = 15, face = "bold"),
          axis.title.y = element_markdown(size = 15, face = "bold", lineheight = 1.1, halign = 0.5)) +
    scale_color_manual(values = c(
        "RCP 2.6" = color_RCP[1],
        "RCP 4.5" = color_RCP[2],
        "RCP 6.0" = color_RCP[3],
        "RCP 8.5" = color_RCP[4]
    )) -> p_wadaptation_boxplot


figure2_data_clean %>% 
    filter(adaptation == "Heat with adaptation" & scenario == "RCP 2.6") %>% 
    ggplot(aes(x = GDP_per_capita_future, y = DALY_capita)) +
    geom_point(aes(color = region), size = 4) +
    geom_text_repel(
        data = figure2_data_clean %>% 
            filter(adaptation == "Heat with adaptation" & scenario == "RCP 2.6"),
        aes(label = country), max.overlaps = 10, seed = 1, size = 6) + 
    geom_hline(aes(yintercept = mean(DALY_capita)), color = "darkgrey", linewidth = 1.3) + 
    geom_vline(aes(xintercept = mean(GDP_per_capita_future)), color = "darkgrey", linewidth = 1.3) + 
    scale_shape_manual(values = 15:18) +
    theme_bw() + 
    guides(color = guide_legend(title = "Region")) + 
    scale_x_continuous(breaks = seq(0, 140000, 10000), limits = c(0, 132000),
                       labels = scales::label_number(suffix = "k", scale = 1e-3)) +
    scale_y_continuous(breaks = seq(-0.1, 0.1, 0.005), limits = c(-0.006, 0.026)) +
    scale_color_manual(values = colors_order) + 
    xlab(expression(bold("Per-capita GDP" ~"(USD)"))) + 
    ylab("**Per-capita Damage Factor**<br>(**DALY·°C<sup>-1</sup>·yr<sup>-1</sup>·cap<sup>-1</sup>**)") +
    ggtitle("Heat with adaptation") + 
    theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
    annotate(geom = "plot",
             x = I(1),
             y = I(1),
             label = p_wadaptation_boxplot,
             vp.width = 0.6,
             vp.height = 0.6) -> figure2_heat_w_adaptation


figure2_total <- 
    figure2_heat_wo_adaptation /
    figure2_heat_w_adaptation +
    plot_annotation(tag_levels = "A") + 
    plot_layout(guides = 'collect') &
    theme(legend.position = 'right', 
          legend.box.just = "left",
          legend.box = "vertical",
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          axis.title.y = element_markdown(size = 20, face = "bold", lineheight = 1.1, halign = 0.5),
          axis.title.x = element_text(size = 20, face = "bold"),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20, face = "bold"),
          plot.tag = element_text(size = 30, face = "bold"))


ggsave(filename = "main_Figure2.png", plot = figure2_total, width = 15, height = 15)



#### Figure 3 (injustice) ####
figure3A_data <- readxl::read_xlsx("area_graph_inducer_updated.xlsx")

figure3A_data %>% 
    select(1:11) %>% 
    janitor::row_to_names(1) %>%
    filter(!is.na(Label)) %>% 
    pull(Label) -> fig3A_region_order

figure3A_data %>% 
    select(1:11) %>% 
    janitor::row_to_names(1) %>% 
    mutate(across(`population fraction %`:area, as.numeric)) %>% 
    filter(!is.na(Label)) %>%
    select(-`DALY per capita`, -area) %>% 
    gather(key = harm_to, value = DALY_capita, `South Asia`:`North America`) %>% 
    rename(inducer = Label,
           population_perc = `population fraction %`) %>% 
    mutate(inducer = factor(inducer, levels = fig3A_region_order),
           harm_to = factor(harm_to, levels = fig3A_region_order)) -> figure3A_data_clean

figure3A_data_clean %>% 
    distinct(inducer, .keep_all = T) %>% 
    mutate(pop_cumsum = cumsum(population_perc)) -> fig3A_cumsum_result_population

figure3A_data_clean %>% 
    arrange(inducer, harm_to) %>% 
    group_by(inducer) %>% 
    mutate(DALY_cumsum = cumsum(DALY_capita)) %>% 
    ungroup() -> fig3A_cumsum_result_DALY


figure3A_data_clean %>% 
    arrange(inducer, harm_to) %>% 
    mutate(right = fig3A_cumsum_result_population$pop_cumsum[match(inducer, fig3A_cumsum_result_population$inducer)],
           left = right - population_perc,
           up = fig3A_cumsum_result_DALY$DALY_cumsum,
           down = up - DALY_capita) %>% 
    mutate(up = ifelse(DALY_capita < 0, 0, up),
           down = ifelse(DALY_capita < 0, DALY_capita, down)) -> figure3A_data_final

figure3A_data_final %>% 
    filter(down > 0) %>% 
    group_by(inducer) %>% 
    slice(n()) -> fig3A_text_positioning


colors_order <- pal_aaas("default")(7)
colors_order <- c(
    "Sub-Saharan Africa" = colors_order[2],
    "South Asia" = colors_order[6],
    "Latin America & Caribbean" = colors_order[3],
    "East Asia & Pacific" = colors_order[4],
    "Middle East & North Africa" = colors_order[5],
    "Europe & Central Asia" = colors_order[1],
    "North America" = colors_order[7]
)

frame_data_A <- figure3A_data_final %>% 
    filter(inducer == "North America", harm_to == "Sub-Saharan Africa")

# Arrow start and end points
arrow_start_x_A <- 110
arrow_start_y_A <- 0.03
arrow_end_x_A <- frame_data_A$left + (frame_data_A$right - frame_data_A$left) / 2 # Center of the NA bar
arrow_end_y_A <- frame_data_A$down + (frame_data_A$up - frame_data_A$down) / 2   # Mid-height of the red segment

# --- PLOT CODE FOR FIGURE 3A ---
figure3A_data_final %>% 
    ggplot() +
    geom_rect(aes(xmin = left, xmax = right, ymin = down, ymax = up, fill = harm_to)) +
    # --- ANNOTATIONS ---
    # 1. Main "Induced by" label with yellow background
    annotate("label", x = 97, y = 0.11, label = "Induced by", hjust = 0.5, size = 10, 
             fontface = "bold", fill = "yellow", color = "black",
             alpha = 0.7) +
    
    # 2. Add the black frame around the specific segment
    geom_rect(data = frame_data_A, 
              aes(xmin = left, xmax = right, ymin = down, ymax = up), 
              color = "black", fill = NA, linewidth = 1) +
    
    # 3. Boxed annotation, now with black text and border
    annotate("richtext", x = 125, y = 0.04, 
             label = "Suffered<br>in <span style='color:#EE0000FF; font-weight:bold;'>Sub-Saharan Africa</span><br>from <span style='color:#5F559BFF; font-weight:bold;'>North America</span>", 
             hjust = 0.5, size = 8, label.color = "black") + 
    
    # 4. Arrow, now black
    annotate("segment", 
             x = arrow_start_x_A, 
             y = arrow_start_y_A, 
             xend = arrow_end_x_A, 
             yend = arrow_end_y_A,
             arrow = arrow(type = "closed", length = unit(0.03, "npc")),
             color = "black", linewidth = 1) + 
    
    geom_label_repel(data = fig3A_text_positioning, 
                     aes(x = right - (right - left) * 0.5, 
                         y = up,
                         label = inducer,
                         color = inducer),
                     direction = "y",
                     force = 8,
                     nudge_y = 0.005,
                     size = 7
    ) +
    xlab("Population fraction (%)") +
    ylab("**Induced health impacts**<br>(**DALY·cap<sup>-1</sup>·yr<sup>-1</sup>**)") + 
    labs(fill = "Region") + 
    coord_cartesian(xlim = c(0, 100), clip = "off") + 
    theme_classic() +
    scale_fill_manual(values = colors_order) +
    scale_color_manual(guide = "none",
                       values = colors_order) + 
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", linewidth = 1) + 
    scale_x_continuous(breaks = seq(0, 100, 10)) +
    scale_y_continuous(limits = c(-0.001, 0.12), 
                       breaks = seq(-0.2, 0.1, 0.02)) -> figure3A



# Figure 3B receptor
figure3B_data <- readxl::read_xlsx("area_graph_receptor_updated.xlsx")


figure3B_data %>% 
    select(1:11) %>% 
    janitor::row_to_names(1) %>%
    clean_names() %>% 
    filter(!is.na(label)) %>%
    rowwise() %>% 
    mutate(damage_value = sum(south_asia:north_america)) %>%
    arrange(-damage_value) %>%
    pull(label) -> fig3B_region_order

figure3B_data %>% 
    select(1:11) %>% 
    janitor::row_to_names(1) %>% 
    mutate(across(`population fraction %`:area, as.numeric)) %>% 
    filter(!is.na(Label)) %>%
    select(-`DALY per capita`, -area) %>% 
    gather(key = harm_from, value = DALY_capita, `South Asia`:`North America`) %>% 
    rename(receptor = Label,
           population_perc = `population fraction %`) %>% 
    mutate(receptor = factor(receptor, levels = fig3B_region_order),
           harm_from = factor(harm_from, levels = fig3B_region_order)) -> figure3B_data_clean


figure3B_data_clean %>%
    distinct(receptor, .keep_all = T) %>% 
    arrange(-DALY_capita) %>% 
    mutate(pop_cumsum = cumsum(population_perc)) -> fig3B_cumsum_result_population

figure3B_data_clean %>% 
    arrange(receptor, harm_from) %>% 
    group_by(receptor) %>% 
    mutate(DALY_cumsum = cumsum(DALY_capita)) %>% 
    ungroup() -> fig3B_cumsum_result_DALY


figure3B_data_clean %>% 
    arrange(receptor, harm_from) %>% 
    mutate(right = fig3B_cumsum_result_population$pop_cumsum[match(receptor, fig3B_cumsum_result_population$receptor)],
           left = right - population_perc,
           up = fig3B_cumsum_result_DALY$DALY_cumsum,
           down = up - DALY_capita) %>% 
    mutate(up = ifelse(DALY_capita < 0, 0, up),
           down = ifelse(DALY_capita < 0, DALY_capita, down),
           harm_from = factor(harm_from, levels = fig3A_region_order)) -> figure3B_data_final

figure3B_data_final %>% 
    filter(down > 0) %>% 
    group_by(receptor) %>% 
    slice(n()) -> fig3B_text_positioning

fig3B_text_positioning %>% 
    bind_rows(figure3B_data_final %>% 
                  filter(receptor == "North America" & down == min(down))) -> fig3B_text_positioning


# --- ARROW AND FRAME COORDINATES FOR FIGURE 3B ---
frame_data_B <- 
    figure3B_data_final %>% 
    filter(receptor == "Sub-Saharan Africa", harm_from == "North America")

# Arrow start and end points
arrow_start_x_B <- 25.5
arrow_start_y_B <- 0.074
arrow_end_x_B <- frame_data_B$left + (frame_data_B$right - frame_data_B$left) / 2 
arrow_end_y_B <- frame_data_B$down + (frame_data_B$up - frame_data_B$down) / 2

figure3B_data_final %>% 
    ggplot() +
    geom_rect(aes(xmin = left, xmax = right, ymin = down, ymax = up, fill = harm_from)) +
    
    # --- ANNOTATIONS ---
    # 1. Main "Suffered in" label with yellow background
    annotate("label", x = 8, y = 0.085, label = "Suffered in", hjust = 0.5, size = 10, 
             fontface = "bold", fill = "yellow", alpha = 0.7, color = "black") +
    
    # 2. Add the black frame around the specific segment
    geom_rect(data = frame_data_B, 
              aes(xmin = left, xmax = right, ymin = down, ymax = up), 
              color = "black", fill = NA, linewidth = 1) +
    
    # 3. Boxed annotation, now with black text and border
    annotate("richtext",
             x = 42, # Adjusted x for better placement
             y = 0.075, 
             label = "Induced<br>by <span style='color:#5F559BFF; font-weight:bold;'>North America</span><br>in <span style='color:#EE0000FF;'>Sub-Saharan Africa</span>", 
             hjust = 0.5, size = 8, label.color = "black") +
    
    # 4. Arrow, now black
    annotate("segment", 
             x = arrow_start_x_B, 
             y = arrow_start_y_B, 
             xend = arrow_end_x_B, 
             yend = arrow_end_y_B,
             arrow = arrow(type = "closed", length = unit(0.04, "npc")),
             color = "black", linewidth = 1) +
    
    geom_label_repel(data = fig3B_text_positioning, 
                     aes(x = right - (right - left) * 0.5, 
                         y = up,
                         label = receptor,
                         color = receptor),
                     direction = "y",
                     force = 8,
                     nudge_y = 0.005,
                     size = 7
    ) +
    xlab("Population fraction (%)") + 
    ylab("**Suffered health impacts**<br>(**DALY·cap<sup>-1</sup>·yr<sup>-1</sup>**)") + 
    labs(fill = "Region") +
    theme_classic() +
    scale_fill_manual(values = colors_order) +
    scale_color_manual(guide = "none",
                       values = colors_order) + 
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", linewidth = 1) + 
    scale_x_continuous(breaks = seq(0, 100, 10)) +
    scale_y_continuous(limits = c(-0.001, 0.1), 
                       breaks = seq(-0.2, 0.1, 0.02)) -> figure3B


figure3A_data_final %>% 
    group_by(inducer) %>% 
    summarise(total_DALY_induce = sum(DALY_capita)) -> figure3C_total_DALY_induce

figure3B_data_final %>% 
    group_by(receptor) %>% 
    summarise(total_DALY_receive = sum(DALY_capita)) -> figure3C_total_DALY_receive

figure3C_total_DALY_induce %>% 
    left_join(figure3C_total_DALY_receive, by = c("inducer" = "receptor")) %>% 
    mutate(injustice_DALY = total_DALY_receive - total_DALY_induce) -> figure3C_injustice_DALY


figure3C_injustice_DALY %>% 
    left_join(figure3A_data_final %>% 
                  select(inducer, right, left) %>% 
                  distinct(inducer, .keep_all = T),
              by = "inducer") %>% 
    relocate(left, .before = right) %>% 
    mutate(up = ifelse(injustice_DALY > 0, injustice_DALY, 0),
           down = ifelse(injustice_DALY < 0, injustice_DALY, 0)) -> figure3C_injustice_DALY_final


figure3C_injustice_DALY_final %>% 
    ggplot() +
    geom_rect(aes(xmin = left, xmax = right, ymin = down, ymax = up, fill = inducer)) +
    geom_label_repel(aes(x = right - (right - left) * 0.5,
                         y = ifelse(injustice_DALY > 0, up, down),
                         label = inducer,
                         color = inducer),
                     direction = "y",
                     force = 8,
                     nudge_y = ifelse(figure3C_injustice_DALY_final$injustice_DALY >= 0, 0.005, -0.005),
                     size = 7
    ) +
    # "High injustice suffered in" text
    annotate("label", x = 50, y = 0.06, label = "High injustice\nsuffered in", hjust = 0.5, size = 10, 
             fontface = "bold", fill = "yellow", alpha = 0.7, color = "black") +
    # "High injustice induced by" text
    annotate("label", x = 50, y = -0.08, label = "High injustice\ninduced by", hjust = 0.5, size = 10, 
             fontface = "bold", fill = "yellow", alpha = 0.7, color = "black") +
    xlab("Population fraction (%)") + 
    ylab("**Injustice index**<br>(**DALY·cap<sup>-1</sup>·yr<sup>-1</sup>**)") + 
    labs(fill = "Region") + 
    coord_cartesian(xlim = c(0, 100), clip = "off") + 
    theme_classic() +
    scale_fill_manual(values = colors_order) +
    scale_color_manual(guide = "none",
                       values = colors_order) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", linewidth = 1) + 
    scale_x_continuous(breaks = seq(0, 100, 10)) +
    scale_y_continuous(limits = c(-0.1, 0.07), 
                       breaks = seq(-0.1, 0.1, 0.02)) -> figure3C


layout <- 
"
AA
BB
CC
"

figure3A + figure3B + figure3C +
    plot_annotation(tag_levels = "A") + 
    plot_layout(guides = "collect", design = layout) &
    theme(legend.position = "right",
          legend.direction = "vertical",
          plot.tag = element_text(size = 30, face = "bold"),
          axis.text.x = element_text(size = 25),
          axis.text.y = element_text(size = 25),
          axis.title.x = element_text(size = 30, face = "bold"),
          axis.title.y = element_markdown(size = 30, face = "bold", lineheight = 1.1, halign = 0.5),
          legend.text = element_text(size = 25),
          legend.title = element_text(size = 30, face = "bold")) -> figure3_total


ggsave(filename = "main_Figure3.png", figure3_total, width = 18, height = 22)



#### Figure 4 - disparity map ####
figure4_data <- readxl::read_xlsx("disparity_map_updated.xlsx")

figure4_data %>% 
    janitor::row_to_names(1) %>% 
    rename(country = Region,
           injustice = `Total DALY/kgCO2eq.y-1 per capita`) %>% 
    mutate(injustice = as.numeric(injustice)) -> figure4_data_clean

figure4_data_clean
figure4_country_names <- read.csv("figure_country_names.csv") %>% as_tibble()
figure4_country_names %>% mutate(ISO3 = gsub(pattern = " ", replacement = "", ISO3)) -> figure4_country_names
figure4_data_clean %>% 
    left_join(figure4_country_names %>% select(-X), by = c("country" = "country")) -> figure4_data_clean


# Load the world map and convert it to an sf object
world_map <- st_as_sf(getMap(resolution = "low"))
# CHECK; should be 0
sum(!(figure4_data_clean$ISO3 %in% world_map$ISO_A3))

# Merge the data with the world map
fig4_map_data <- left_join(world_map, figure4_data_clean %>% select(ISO3, injustice), by = c("ISO_A3" = "ISO3")) %>% as_tibble()
# Select relevant columns to retain geometry and metric columns for melting
fig4_map_data_selected <- 
    fig4_map_data %>%
    select(geometry, ISO_A3, injustice)

# Reshape data for plotting
fig4_map_data_long <- 
    fig4_map_data_selected %>% 
    gather(key = "Metric", value = "Value", injustice) %>% 
    mutate(Metric = as.factor(Metric))

# Plot all three maps in one figure using facet_wrap
ggplot(fig4_map_data_long) +
    geom_sf(aes(fill = Value, geometry = geometry)) +
    scale_fill_steps2(low = "blue", mid = "white", high = "red",
                      midpoint = 0,
                      n.breaks = 11,
                      nice.breaks = T,
                      na.value = "grey50",
                      name = "**Injustice Net Damage**<br>(**DALY·cap<sup>-1</sup>·yr<sup>-1</sup>**)") +
    coord_sf(ylim = c(-60, 90), expand = F) + # remove Antarctica for better visibility
    theme_minimal() +
    theme(
        legend.position = "bottom",
        legend.title = element_markdown(size = 20, vjust = 0.9, lineheight = 0.9, halign = 0.5), # Use element_markdown
        legend.text = element_text(size = 15),
        panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
        axis.text = element_blank(),         # Remove axis text
        axis.ticks = element_blank(),          # Remove axis ticks
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")
    ) +
    guides(
        fill = guide_colourbar(theme = theme(
            legend.key.width = unit(20, "cm")))) -> figure4_map

ggsave(filename = "main_Figure4.png", figure4_map, width = 15, height = 10)

