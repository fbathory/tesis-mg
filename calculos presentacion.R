library(tidyverse)
library(openxlsx)
library(cowplot)

# Resultados e20 para regraficar por sexo
e20_escenarios_p <- read.xlsx("data/e20 3 escenarios para presentar.xlsx",sep.names = " ")
e20_escenarios_b <- e20_escenarios_p %>% 
  pivot_longer(cols = 4:9, names_to = "escenario_anio", values_to = "e20") %>% 
  mutate(Año = case_when(escenario_anio %in% c("e20 observada 2020", "e20 sin COVID 2020", "e20 esperada 2020") ~ "2020",
                         TRUE                                                                                   ~ "2021"),
         Escenario = case_when(escenario_anio %in% c("e20 observada 2020", "e20 observada 2021") ~ "observado con COVID-19",
                               escenario_anio %in% c("e20 sin COVID 2020", "e20 sin COVID 2021") ~ "observado sin COVID-19",
                               escenario_anio %in% c("e20 esperada 2020","e20 esperada 2021")    ~ "proyectado sin COVID-19")) %>% 
  pivot_wider(names_from = Sexo, values_from = e20)
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

p_e20_mujeres <- e20_escenarios_b %>%
  select(1:6) |>
  mutate(Jurisdicción = case_when(Jurisdicción == "Ciudad Autónoma de Buenos Aires" ~ "CABA",
                         Jurisdicción == "Tierra del Fuego, Antártida e Islas del Atlántico Sur" ~ "TDF",
                         Jurisdicción == "Santiago del Estero" ~ "SDE",
                         Jurisdicción == "Total" ~ "Total del país",
                         TRUE ~ Jurisdicción)) %>%
  ggplot(aes(x = reorder(Jurisdicción,-Mujer*(Escenario=="observado con COVID-19" & Año == "2021")))) +
  geom_rect(xmin=13.5, xmax=14.5, ymin=-Inf, ymax=Inf, fill = "grey", alpha=0.01) +
  geom_point(aes(y = Mujer, color = Escenario, shape = Escenario), alpha = 0.7, size = 3.5)+
  theme_bw()+
  coord_flip()+
  facet_grid(~Año, scales = "free_x")+
  theme(panel.background = element_rect(fill = 'white'), # Fondo del panel blanco
        plot.background = element_rect(fill = 'transparent', color = NA), # Fondo del gráfico transparente
        legend.background = element_rect(fill = 'transparent'),
        legend.position = "top",
        strip.background = element_rect(fill = "white"), # Fondo de las etiquetas de faceta
        strip.text = element_text(color = "black"), # Texto de las etiquetas de faceta
        panel.border = element_rect(color = "white"), # Borde del panel
        legend.title = element_blank(),
        axis.text.y = element_text(face = c(rep("plain", 13), "bold", rep("plain", 8)), color = "white"),
        axis.text.x = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        axis.ticks = element_line(color = "white"),
        text = element_text(color = "white"),
        strip.text.x = element_text(color = "black") )+
  scale_colour_manual(name = "Escenario",
                      values = c("#540D6E","#EDAE49","#CE5374")) +
  scale_shape_manual(name = "Escenario",
                     values = c(16,17,18))+
  labs(color = "",
       y = expression(paste("e" ["20"])),
       x = "Jurisdicción")+
  ylim(55,66)
# panel.background = element_rect(fill='transparent'), #transparent panel bg
# plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
# panel.grid.major = element_blank(), #remove major gridlines
# panel.grid.minor = element_blank(), #remove minor gridlines
# legend.background = element_rect(fill='transparent'), #transparent legend bg
# legend.box.background = element_rect(fill='transparent') #transparent legend panel

ggsave(plot = p_e20_mujeres, filename = "images/e20mujeres.png", 
       width = 1300, height = 1000, units = "px", dpi = 180,bg = "transparent")

p_e20_varones <- e20_escenarios_b %>%
  select(1:5, 7) |>
  mutate(Jurisdicción = case_when(Jurisdicción == "Ciudad Autónoma de Buenos Aires" ~ "CABA",
                         Jurisdicción == "Tierra del Fuego, Antártida e Islas del Atlántico Sur" ~ "TDF",
                         Jurisdicción == "Santiago del Estero" ~ "SDE",
                         Jurisdicción == "Total" ~ "Total del país",
                         TRUE ~ Jurisdicción)) %>%
  ggplot(aes(x = reorder(Jurisdicción,-Varón*(Escenario=="observado con COVID-19" & Año == "2021")))) +
  geom_rect(xmin=12.5, xmax=13.5, ymin=-Inf, ymax=Inf, fill = "grey", alpha=0.01) +
  geom_point(aes(y = Varón, color = Escenario, shape = Escenario), alpha = 0.7, size = 3.5)+
  theme_bw()+
  coord_flip()+
  facet_grid(~Año, scales = "free_x")+
  theme(panel.background = element_rect(fill = 'white'), # Fondo del panel blanco
        plot.background = element_rect(fill = 'transparent', color = NA), # Fondo del gráfico transparente
        legend.background = element_rect(fill = 'transparent'),
        legend.position = "top",
        strip.background = element_rect(fill = "white"), # Fondo de las etiquetas de faceta
        strip.text = element_text(color = "black"), # Texto de las etiquetas de faceta
        panel.border = element_rect(color = "white"), # Borde del panel
        legend.title = element_blank(),
        axis.text.y = element_text(face = c(rep("plain", 12), "bold", rep("plain", 8)), color = "white"),
        axis.text.x = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        axis.ticks = element_line(color = "white"),
        text = element_text(color = "white"),
        strip.text.x = element_text(color = "black") )+
  scale_colour_manual(name = "Escenario",
                      values = c("#540D6E","#EDAE49","#CE5374")) +
  scale_shape_manual(name = "Escenario",
                     values = c(16,17,18))+
  labs(color = "",
       y = expression(paste("e" ["20"])),
       x = "Jurisdicción")+
  ylim(48,61)

ggsave(plot = p_e20_varones, filename = "images/e20varones.png", 
       width = 1300, height = 1000, units = "px", dpi = 180,bg = "transparent")

# plot_grid(p_e20_mujeres, p_e20_varones, ncol = 2)

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
  mutate(Jurisdicción = "Total del país") |>
  ggplot(aes(x = x, y = Valor, color = Causa, fill = Causa))+
  geom_col(aes(alpha = ifelse(Valor > 0, 0.5, 0.9)))+
  # facet_grid(geo ~ sexo + Causa, scales = "free")
  facet_grid(sexo ~ Jurisdicción, switch = "y")+
  theme_bw()+
  # scale_x_continuous(breaks = c(seq(20,80,10)))+
  scale_alpha_continuous(guide = FALSE)+
  scale_fill_manual(name = "Causas", values = c("#EDAE49", "#CE5374"))+
  scale_color_manual(name = "Causas", values = c("#EDAE49", "#CE5374"))+
  theme(panel.background = element_rect(fill = 'white'), # Fondo del panel blanco
        plot.background = element_rect(fill = 'transparent', color = NA), # Fondo del gráfico transparente
        legend.background = element_rect(fill = 'transparent'),
        legend.position = "top",
        strip.background = element_rect(fill = "white"), # Fondo de las etiquetas de faceta
        strip.text = element_text(color = "black"), # Texto de las etiquetas de faceta
        panel.border = element_rect(color = "white"), # Borde del panel
        # legend.title = element_blank(),
        axis.text.y = element_text(face = c(rep("plain", 13), "bold", rep("plain", 8)), color = "white"),
        axis.text.x = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        axis.ticks = element_line(color = "white"),
        text = element_text(color = "white"),
        strip.text.x = element_text(color = "black") 
        #axis.text.x = element_text(angle = -270, vjust = 0, size = 10)
        )+
  labs(x = "Edad",
       y = expression(paste("Diferencia de e" ["20"], " 2019-2021")))

ggsave(plot = p_decomp_pais, filename = "images/decomp_totalpais.png", 
       width = 1300, height = 1000, units = "px", dpi = 215,bg = "transparent")

p_decomp_selec <- res_decomp_20 |>
  left_join(geo, by = c("geo" = "codprov")) |>
  arrange(geo) |>
  pivot_longer(cols = Otras:COVID, names_to = "Causa", values_to = "Valor") |>
  filter(geo %in% c(26,30,38,62,58,34)) |>
  mutate(Jurisdicción = factor(Jurisdicción,
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
  scale_fill_manual(name = "Causas", values = c("#EDAE49", "#CE5374"))+
  scale_color_manual(name = "Causas", values = c("#EDAE49", "#CE5374"))+
  theme(panel.background = element_rect(fill = 'white'), # Fondo del panel blanco
        plot.background = element_rect(fill = 'transparent', color = NA), # Fondo del gráfico transparente
        legend.background = element_rect(fill = 'transparent'),
        legend.position = "top",
        strip.background = element_rect(fill = "white"), # Fondo de las etiquetas de faceta
        strip.text = element_text(color = "black"), # Texto de las etiquetas de faceta
        panel.border = element_rect(color = "white"), # Borde del panel
        # legend.title = element_blank(),
        axis.text.y = element_text(face = c(rep("plain", 13), "bold", rep("plain", 8)), color = "white"),
        axis.text.x = element_text(color = "white", angle = 90, vjust = 0, size = 10),
        axis.title = element_text(color = "white"),
        axis.ticks = element_line(color = "white"),
        text = element_text(color = "white"),
        strip.text.x = element_text(color = "black")#, 
        # axis.text.x = element_text(angle = 90, vjust = 0, size = 10)
  )+
  labs(x = "Edad",
       y = expression(paste("Diferencia de e" ["20"], " 2019-2021")))

ggsave(plot = p_decomp_selec, filename = "images/decomp_selec.png", 
       width = 1600, height = 900, units = "px", dpi = 215,bg = "transparent")

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

pob_def <- openxlsx::read.xlsx("C:/Users/Usuario/Desktop/Calculos tesis/Export/base_defunciones_pob.xlsx")
source("C:/Users/Usuario/Desktop/Calculos tesis/LT20.R")

pob_20 <- pob_def %>% 
  filter(anio == 2019) %>% 
  ungroup() %>% 
  group_by(codgeo, geo, edad5) %>% 
  summarise(n = sum(n),
            def = sum(def),
            tasa_def = def/n) %>% 
  mutate(edadx = as.numeric(substr(edad5, start = 2, stop = 3)))

x <- c(seq(from = 20, to = 80, by = 5))

for (p in unique(pob_def$codgeo)){
    
    prov <- pob_20 |>  filter(codgeo == p)
      
      lt_data <- prov %>% ungroup()
      
      # lt_base <- LifeTable(x = x,
      #     mx = lt_data$tasa_def,
      #     sex = ifelse(unique(lt_data$sexo) == "Mujer", "female", "male"))
      
      lt <- lifetable20(deaths = lt_data$def,
                        pop    = lt_data$n, 
                        Age    = lt_data$edadx) |> 
        mutate(geo = p)
      
      
      #con esto las creo en el environment
      assign(paste0("lt_2019", "_", p, "_"),
             lt,
             envir = .GlobalEnv)
      
      # assign(paste0("nMx_", a, "_", p, "_", s),
      #      geo %>% ungroup() |>  filter (sexo == s),
      #      envir = .GlobalEnv)
      
      }
nombres <- ls(pattern = "lt_20")

# Armo lista con todas las LT observadas
lst <- mget(nombres)

obtener_valor_y_nombre <- function(df, nombre) {
  valor <- df[1, 11]  
  df_resultado <- data.frame(NombreDataFrame = nombre, Valor = valor)
  return(df_resultado)
}
e20_list <- lapply(seq_along(lst), function(i) {
  obtener_valor_y_nombre(lst[[i]], nombres[i])
})

e20 <- do.call(rbind, e20_list) |> 
  mutate(#extraigo año de NombreDataFrame
    anio    = substr(NombreDataFrame, start = 4, stop = 7),
    
    #extraigo código de provincia
    codprov = substr(NombreDataFrame, start = 9, stop = 10),
    
    # transformo el código para CABA y PBA
    codprov = case_when(codprov == "2_" ~ "2",
                        codprov == "6_" ~ "6",
                        codprov == "0_" ~ "0",
                        TRUE            ~ codprov),
    #transformo codprov a numérico
    codprov = as.numeric(codprov)) |> 
  select(-NombreDataFrame) |> 
  pivot_wider(names_from = anio, values_from = Valor,names_prefix = "obs_") |> 
  
  #incorporo nombre de provincia
  left_join(geo,
            by = "codprov") |>
  relocate(Jurisdicción, .after = codprov) |>
  arrange(codprov)

openxlsx::write.xlsx(e20,"data/e20_2019.xlsx")
