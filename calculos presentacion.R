library(tidyverse)
library(openxlsx)
library(cowplot)

# Resultados e20 para regraficar por sexo
e20_escenarios_p <- read.xlsx("data/e20 3 escenarios para presentar.xlsx",sep.names = " ")

# Tengo que cambiar el año por el sexo para facetear. Tengo que rearmar columnas

# p_e20_2020 <- e20_escenarios %>%
#   select(1:3, 
#          "observado con COVID-19" = e20_2020,
#          "observado sin COVID-19" = e20cf_2020,
#          "proyectado sin COVID-19"= e20esp_2020) |> 
#   pivot_longer(cols = `observado con COVID-19`:`proyectado sin COVID-19`,names_to = "escenario", values_to = "e20") %>% 
#   mutate(geo = case_when(geo == "Ciudad Autónoma de Buenos Aires" ~ "CABA",
#                          geo == "Tierra del Fuego, Antártida e Islas del Atlántico Sur" ~ "TDF",
#                          geo == "Total" ~ "Total del país",
#                          TRUE ~ geo)) %>%
#   ggplot(aes(x = reorder(geo,-e20*(escenario=="observado con COVID-19")))) +
#   geom_rect(xmin=15.5, xmax=16.5, ymin=-Inf, ymax=Inf, fill = "grey", alpha=0.01) +
#   geom_point(aes(y = e20, color = escenario, shape = escenario), alpha = 0.7, size = 3.5)+
#   theme_bw()+
#   coord_flip()+
#   facet_grid(~sexo, scales = "free_x")+
#   theme(legend.position = "top",
#         strip.background =element_rect(fill="transparent"),
#         legend.title = element_blank(),
#         axis.text.y = element_text(face = c(rep("plain",15), "bold", rep("plain", 8))))+
#   scale_colour_manual(name = "Escenario",
#                       values = c("#F8766D","#00BA38","#619CFF")) +
#   scale_shape_manual(name = "Escenario",
#                      values = c(16,17,18))+
#   labs(color = "",
#        y = expression(paste("e" ["20"], " 2020")),
#        x = "Jurisdicción")+
#   ylim(50,66)

geo_total = data.frame(codprov = 0, Jurisdicción="Total") |> 
  mutate(codprov = as.numeric(codprov))
geo <- e20_escenarios_p[,1:2] %>% 
  rbind(geo_total) |> 
  distinct() 

# Resultados descomposición para rearmar gráfico - grupos veintenales
res_decomp_p <- read.xlsx("data/resultados descomposición base.xlsx")
res_decomp_20 <- res_decomp_p %>% 
  mutate(x = case_when(x %in% c(20,25,30,35) ~ "20-39",
                       x %in% c(40,45,50,55) ~ "40-59",
                       x %in% c(60,65,70,75) ~ "60-79",
                       x == 80               ~ "80+"),
         x = factor(x, levels = c("20-39",
                                  "40-59",
                                  "60-79",
                                  "80+"))) %>% 
  group_by(x, geo, sexo) %>% 
  summarise(COVID = sum(COVID),
            Otras = sum(Otras))

p_decomp_pais <- res_decomp_20 |>
  left_join(geo, by = c("geo" = "codprov")) |>
  arrange(geo) |>
  pivot_longer(cols = Otras:COVID, names_to = "Causa", values_to = "Valor") |>
  filter(geo %in% c(0)) |>
  mutate(Jurisdicción = case_when(geo == 2  ~ "CABA",
                                  geo == 6  ~ "PBA",
                                  geo == 86 ~ "SDE",
                                  geo == 94 ~ "TDF",
                                  geo == 0  ~ "Total del país",
                                  TRUE      ~ Jurisdicción),
         Jurisdicción = factor(Jurisdicción,
                               levels = c("Total del país",
                                          "CABA",
                                          "PBA",
                                          "Catamarca",
                                          "Córdoba",
                                          "Corrientes",
                                          "Chaco",
                                          "Chubut",
                                          "Entre Ríos",
                                          "Formosa",
                                          "Jujuy",
                                          "La Pampa",
                                          "La Rioja",
                                          "Mendoza",
                                          "Misiones",
                                          "Neuquén",
                                          "Río Negro",
                                          "Salta",
                                          "San Juan",
                                          "San Luis",
                                          "Santa Cruz",
                                          "Santa Fe",
                                          "SDE",
                                          "Tucumán",
                                          "TDF"))) |>
  ggplot(aes(x = x, y = Valor, color = Causa, fill = Causa))+
  geom_col(aes(alpha = ifelse(Valor > 0, 0.5, 0.9)))+
  # facet_grid(geo ~ sexo + Causa, scales = "free")
  facet_grid(sexo ~ Jurisdicción, switch = "y")+
  theme_bw()+
  # scale_x_continuous(breaks = c(seq(20,80,10)))+
  scale_alpha_continuous(guide = FALSE)+
  theme(strip.background = element_rect(fill="transparent"),
        # legend.title   = element_blank(),
        legend.position = "top",
        axis.text.x = element_text(angle = -270, vjust = 0, size = 10))+
  labs(x = "Edad",
       y = expression(paste("Diferencia de e" ["20"], " 2019-2021")))

p_decomp1 <- res_decomp_20 |>
  left_join(geo, by = c("geo" = "codprov")) |>
  arrange(geo) |>
  pivot_longer(cols = Otras:COVID, names_to = "Causa", values_to = "Valor") |>
  filter(geo %in% c(2,6,10,14,18,22,26,30,34,38,42,46)) |>
  mutate(Jurisdicción = case_when(geo == 2  ~ "CABA",
                           geo == 6  ~ "PBA",
                           geo == 86 ~ "SDE",
                           geo == 94 ~ "TDF",
                           TRUE      ~ Jurisdicción),
         Jurisdicción = factor(Jurisdicción,
                        levels = c("Total",
                                   "CABA",
                                   "PBA",
                                   "Catamarca",
                                   "Córdoba",
                                   "Corrientes",
                                   "Chaco",
                                   "Chubut",
                                   "Entre Ríos",
                                   "Formosa",
                                   "Jujuy",
                                   "La Pampa",
                                   "La Rioja",
                                   "Mendoza",
                                   "Misiones",
                                   "Neuquén",
                                   "Río Negro",
                                   "Salta",
                                   "San Juan",
                                   "San Luis",
                                   "Santa Cruz",
                                   "Santa Fe",
                                   "SDE",
                                   "Tucumán",
                                   "TDF"))) |>
  ggplot(aes(x = x, y = Valor, color = Causa, fill = Causa))+
  geom_col(aes(alpha = ifelse(Valor > 0, 0.5, 0.9)))+
  # facet_grid(geo ~ sexo + Causa, scales = "free")
  facet_grid(sexo ~ Jurisdicción, switch = "y")+
  theme_bw()+
  # scale_x_continuous(breaks = c(seq(20,80,10)))+
  scale_alpha_continuous(guide = FALSE)+
  theme(strip.background = element_rect(fill="transparent"),
        # legend.title   = element_blank(),
        legend.position = "top",
        axis.text.x = element_blank())+
  labs(x = "",
       y = "")#expression(paste("Diferencia de e" ["20"], " 2019-2021")))

 
p_decomp2 <- res_decomp_20 |>
  left_join(geo, by = c("geo" = "codprov")) |>
  arrange(geo) |>
  pivot_longer(cols = Otras:COVID, names_to = "Causa", values_to = "Valor") |>
  filter(geo %in% c(50,54,58,62,66,70,74,78,82,86,90,94)) |>
  mutate(Jurisdicción = case_when(geo == 2  ~ "CABA",
                           geo == 6  ~ "PBA",
                           geo == 86 ~ "SDE",
                           geo == 94 ~ "TDF",
                           geo == 0  ~ "Total",
                           TRUE      ~ Jurisdicción),
         Jurisdicción = factor(Jurisdicción,
                        levels = c("Total",
                                   "CABA",
                                   "PBA",
                                   "Catamarca",
                                   "Córdoba",
                                   "Corrientes",
                                   "Chaco",
                                   "Chubut",
                                   "Entre Ríos",
                                   "Formosa",
                                   "Jujuy",
                                   "La Pampa",
                                   "La Rioja",
                                   "Mendoza",
                                   "Misiones",
                                   "Neuquén",
                                   "Río Negro",
                                   "Salta",
                                   "San Juan",
                                   "San Luis",
                                   "Santa Cruz",
                                   "Santa Fe",
                                   "SDE",
                                   "Tucumán",
                                   "TDF"))) |>
  ggplot(aes(x = x, y = Valor, color = Causa, fill = Causa))+
geom_col(aes(alpha = ifelse(Valor > 0, 0.5, 0.9)))+
  # facet_grid(geo ~ sexo + Causa, scales = "free")
  facet_grid(sexo ~ Jurisdicción, switch = "y")+
  theme_bw()+
  # scale_x_continuous(breaks = c(seq(20,80,10)))+
  scale_alpha_continuous(guide = FALSE)+
  scale_color_discrete(guide = "none")+
  scale_fill_discrete(guide = "none")+
  theme(strip.background = element_rect(fill="transparent"),
        # legend.title   = element_blank(),
        # legend.position = NULL,
        axis.text.x = element_text(angle = -270, vjust = 0, size = 10))+
  labs(x = "",
       y = "")#expression(paste("Diferencia de e" ["20"], " 2019-2021")))
# 
p_decomp_prov <- cowplot::plot_grid(p_decomp1, p_decomp2, nrow = 2, align = "v", scale = 0.9)+
  draw_label("Edad", x=0.5, y=  0, vjust=-0.5, angle= 0) +
  draw_label(expression(paste("Diferencia de e" ["20"], " 2019-2021")), x=  0, y=0.5, vjust= 1.5, angle=90)



#+ theme(plot.margin = margin(30, 30, -50, 50))
# p_decomp_prov <- cowplot::add_sub(p_decomp_prov, "Edad", hjust = 1)
# p_decomp_prov <- cowplot::add_sub(p_decomp_prov, expression(paste("Diferencia de e" ["20"], " 2019-2021")), -0.02, 1.8, 
#                                   angle = 90)
# cowplot::ggdraw(p_decomp_prov)
