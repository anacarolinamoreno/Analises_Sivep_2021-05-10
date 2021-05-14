

library(tidyverse)
library(lubridate)
library(tidylog)

# Melhorando a imagem dos gráficos pixelados do Windows

trace(grDevices::png, quote({
  if (missing(type) && missing(antialias)) {
    type <- "cairo-png"
    antialias <- "subpixel"
  }
}), print = FALSE)

# IMPORTANTE: As análises abaixo dependem do código no script sivep_1_preparar_base.R e dos arquivos RDS gerados pelo script sivep_2-1_analises-covid-SP.R

# ÍNDICE DE GRÁFICOS: CASOS DE COVID-19 CONFIRMADA NO ESTADO DE SÃO PAULO (ESP)
# 1.1- Total de internações e mortes por Covid por dia no ESP
# 1.2- Média móvel de internações e mortes por Covid por dia no ESP
# 1.3 - Total de óbitos por Covid por mês para cada faixa etária no ESP

# 1.1- Total de internações e mortes por Covid por dia no ESP


ggplot_covid_sp_dia <- read_rds("data/covid_sp_dia.rds") %>%
  pivot_longer(cols = c(internacoes, mortes), names_to = "categoria", values_to = "valor") %>%
  ggplot(aes(x=data, y=valor, group=categoria, colour=categoria))+
  geom_line(size = 1)+
  labs(
    title = "Internações por SRAG e mortes confirmadas por Covid-19",
    subtitle = "Total por data de ocorrência no Estado de SP",
    caption = "Fonte: Sivep-Gripe (Ministério da Saúde)",
    y = "total por dia",
    x = "data"
  )+
  scale_color_manual(
    values=c("#3D748F", "#a80000"),
    labels = c("Novas internações", "Novas mortes")
  )+
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white", colour="grey"),
    panel.grid.major = element_line(colour = "grey", linetype = "dotted"),
    panel.grid.minor = element_line(colour = "grey", linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.caption = element_text(face = "italic"),
    legend.position="top",
    legend.title = element_blank()
  )+
  # Destaques para mostrar a data e o valor do pico de INTERNAÇÕES
  annotate(
    geom="text",
    x=as.Date("2021-03-01"),
    y=3116,
    label="22/03",
    size = 3) +
  annotate(
    geom="text",
    x=as.Date("2021-03-01"),
    y=2860,
    label="3.116",
    size = 3) +
  annotate(
    geom="point",
    x=as.Date("2021-03-22"),
    y=3116,
    size=3,
    shape=20,
    fill="black")+
  # Destaques para mostrar a data e o valor do pico de MORTES
  annotate(
    geom="text",
    x=as.Date("2021-03-10"),
    y=885,
    label="29/03",
    size = 3) +
  annotate(
    geom="text",
    x=as.Date("2021-03-10"),
    y=690,
    label="885",
    size = 3) +
  annotate(
    geom="point",
    x=as.Date("2021-03-29"),
    y=885,
    size=3,
    shape=20,
    fill="black")+
  # linhas vertical e horizontal
  geom_vline(xintercept=ymd("2021-04-08"), color="grey", size=0.2)+
  # retângulo de fundo cinza indicando o atraso de notificação
  annotate(
    "rect",
    xmin=as.Date("2021-04-08"),
    xmax=as.Date("2021-05-15"),
    ymin=-20,
    ymax=3000,
    alpha=0.2,
    color="transparent",
    fill="grey")+
  # destaque indicanddo o atraso de notificação
  annotate(
    "text",
    x = as.Date("2021-05-10"),
    y = 2100,
    label = "Atraso de notificação",
    color = "#666666",
    size = 3.5,
    angle = -90,
    fontface = "bold"
  )+
  # salvando uma versão do gráfico em formato .SVG para ser facilmente replicado no Illustrator
  ggsave('docs/grafico_covid_sp_dia_totalabsoluto.svg', dpi = 'retina',
         height = 8, width = 12, limitsize = F)


ggplot_covid_sp_dia





# 1.2- Média móvel de internações e mortes por Covid por dia no ESP

ggplot_covid_sp_dia_media <- read_rds("data/covid_sp_dia_media.rds") %>%
  pivot_longer(cols = c(internacoes_mm7d, mortes_mm7d), names_to = "categoria", values_to = "valor") %>%
  ggplot(aes(x=data, y=valor, group=categoria, colour=categoria))+
  geom_line(size = 1)+
  labs(
    title = "Internações por SRAG e mortes confirmadas por Covid-19",
    subtitle = "Média móvel por data de ocorrência no Estado de SP",
    caption = "Fonte: Sivep-Gripe (Ministério da Saúde)",
    y = "total por dia",
    x = "data"
  )+
  scale_color_manual(
    values=c("#3D748F", "#a80000"),
    labels = c("Novas internações", "Novas mortes")
  )+
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white", colour="grey"),
    panel.grid.major = element_line(colour = "grey", linetype = "dotted"),
    panel.grid.minor = element_line(colour = "grey", linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.caption = element_text(face = "italic"),
    legend.position="top",
    legend.title = element_blank()
  )+
  # Destaques para mostrar a data e o valor do pico de INTERNAÇÕES
  annotate(
    geom="text",
    x=as.Date("2021-03-01"),
    y=2891,
    label="24/03",
    size = 3) +
  annotate(
    geom="text",
    x=as.Date("2021-03-01"),
    y=2680,
    label="2.891",
    size = 3) +
  annotate(
    geom="point",
    x=as.Date("2021-03-24"),
    y=2891,
    size=3,
    shape=20,
    fill="black")+
  # Destaques para mostrar a data e o valor do pico de MORTES
  annotate(
    geom="text",
    x=as.Date("2021-03-10"),
    y=857,
    label="29/03",
    size = 3) +
  annotate(
    geom="text",
    x=as.Date("2021-03-10"),
    y=690,
    label="857",
    size = 3) +
  annotate(
    geom="point",
    x=as.Date("2021-03-29"),
    y=857,
    size=3,
    shape=20,
    fill="black")+
  # linhas vertical e horizontal
  geom_vline(xintercept=ymd("2021-04-08"), color="grey", size=0.2)+
  # retângulo de fundo cinza indicando o atraso de notificação
  annotate(
    "rect",
    xmin=as.Date("2021-04-08"),
    xmax=as.Date("2021-05-15"),
    ymin=-20,
    ymax=3000,
    alpha=0.2,
    color="transparent",
    fill="grey")+
  # destaque indicanddo o atraso de notificação
  annotate(
    "text",
    x = as.Date("2021-05-10"),
    y = 2100,
    label = "Atraso de notificação",
    color = "#666666",
    size = 3.5,
    angle = -90,
    fontface = "bold"
  )+
  # salvando uma versão do gráfico em formato .SVG para ser facilmente replicado no Illustrator
  ggsave('docs/grafico_covid_sp_dia_mediamovel.svg', dpi = 'retina',
         height = 8, width = 12, limitsize = F)


ggplot_covid_sp_dia_media

# 1.3 - Total de óbitos por Covid por MÊS para cada faixa etária no ESP

ggplot_obitos_covid_sp_idade_20 <- read_rds("data/obitos_covid_sp_idade_20.rds") %>%
  ggplot(aes(x=mes, y=mortes, group=faixa_etaria, color=faixa_etaria)) +
  # Definindo a forma do gráfico (nesse caso, gráfico de linhas)
  geom_line(size = 1)+
  # Definindo os textos ao redor do gráfico
  labs(
    title = "Mortes confirmadas por Covid-19 (Estado de SP)",
    subtitle = "Total por FAIXA ETÁRIA e por MÊS DO ÓBITO",
    caption = "Fonte: Sivep-Gripe (Ministério da Saúde)",
    y = "total de mortes",
    x = "mês/ano"
  )+
  # Definindo as cores das linhas e os detalhes da legenda
  scale_color_manual(
    values=c("#9398B8", "#8BACBC", "#3D748F", "#a80000", "#7E0F08")
  )+
  geom_point()+
  geom_label(
    aes(label = mortes),
    nudge_y = 0.25,
    size = 2)+
  # Definindo cores e formas dos elementos de fundo do gráfico
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white", colour="grey"),
    panel.grid.major = element_line(colour = "grey", linetype = "dotted"),
    panel.grid.minor = element_line(colour = "grey", linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.caption = element_text(face = "italic"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 8, face = "bold"),
    legend.background = element_rect(fill="#EEEEEE",
                                     size=0.5, linetype="solid")
  )+
  # salvando uma versão do gráfico em formato .SVG para ser facilmente replicado no Illustrator
  ggsave('docs/grafico_obitos_covid_sp_idade_20.svg', dpi = 'retina',
         height = 8, width = 12, limitsize = F)

ggplot_obitos_covid_sp_idade_20

# 1.4 - Total de internações por SRAG por SEMANA para cada faixa etária no ESP

ggplot_internacoes_srag_sp_idade_20 <- read_rds("data/internacoes_srag_sp_idade_20.rds") %>%
  filter(semana > "2020-02-01" & semana < "2021-04-17") %>%
  ggplot(aes(x=semana, y=internacoes, group=faixa_etaria, color=faixa_etaria)) +
  # Definindo a forma do gráfico (nesse caso, gráfico de linhas)
  geom_line(size = 1)+
  # Definindo os textos ao redor do gráfico
  labs(
    title = "Internações por SRAG (Estado de SP)",
    subtitle = "Total por FAIXA ETÁRIA e por SEMANA DA INTERNAÇÃO",
    caption = "Fonte: Sivep-Gripe (Ministério da Saúde)",
    y = "total de internações",
    x = "semana"
  )+
  # Definindo as cores das linhas e os detalhes da legenda
  scale_color_manual(
    values=c("#9398B8", "#8BACBC", "#3D748F", "#a80000", "#7E0F08")
  )+
  geom_point()+
  # Definindo cores e formas dos elementos de fundo do gráfico
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white", colour="grey"),
    panel.grid.major = element_line(colour = "grey", linetype = "dotted"),
    panel.grid.minor = element_line(colour = "grey", linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.caption = element_text(face = "italic"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 8, face = "bold"),
    legend.background = element_rect(fill="#EEEEEE",
                                     size=0.5, linetype="solid")
  )+
  # salvando uma versão do gráfico em formato .SVG para ser facilmente replicado no Illustrator
  ggsave('docs/grafico_internacoes_srag_sp_idade_20.svg', dpi = 'retina',
         height = 8, width = 12, limitsize = F)


ggplot_internacoes_srag_sp_idade_20


# 1.5 - Total de pessoas com primeiros sintomas por SRAG por SEMANA para cada faixa etária no ESP

ggplot_sintomas_srag_sp_idade_20 <- read_rds("data/sintomas_srag_sp_idade_20.rds") %>%
  filter(semana > "2020-02-01" & semana < "2021-04-17") %>%
  ggplot(aes(x=semana, y=novos_doentes, group=faixa_etaria, color=faixa_etaria)) +
  # Definindo a forma do gráfico (nesse caso, gráfico de linhas)
  geom_line(size = 1)+
  # Definindo os textos ao redor do gráfico
  labs(
    title = "Pessoas com primeiros sintomas de SRAG (Estado de SP)",
    subtitle = "Total por FAIXA ETÁRIA e por SEMANA DE SINTOMAS",
    caption = "Fonte: Sivep-Gripe (Ministério da Saúde)",
    y = "total de novos doentes",
    x = "semana"
  )+
  # Definindo as cores das linhas e os detalhes da legenda
  scale_color_manual(
    values=c("#9398B8", "#8BACBC", "#3D748F", "#a80000", "#7E0F08")
  )+
  geom_point()+
  # Definindo cores e formas dos elementos de fundo do gráfico
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white", colour="grey"),
    panel.grid.major = element_line(colour = "grey", linetype = "dotted"),
    panel.grid.minor = element_line(colour = "grey", linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.caption = element_text(face = "italic"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 8, face = "bold"),
    legend.background = element_rect(fill="#EEEEEE",
                                     size=0.5, linetype="solid")
  )+
  # salvando uma versão do gráfico em formato .SVG para ser facilmente replicado no Illustrator
  ggsave('docs/grafico_sintomas_srag_sp_idade_20.svg', dpi = 'retina',
         height = 8, width = 12, limitsize = F)


ggplot_sintomas_srag_sp_idade_20

# 1.6 - Total de mortes por Covid por SEMANA para cada faixa etária no ESP


ggplot_obitos_covid_sp_idade_20_sem <- read_rds("data/obitos_covid_sp_idade_20_sem.rds") %>%
  filter(semana > "2020-02-01" & semana < "2021-05-08") %>%
  ggplot(aes(x=semana, y=mortes, group=faixa_etaria, color=faixa_etaria)) +
  # Definindo a forma do gráfico (nesse caso, gráfico de linhas)
  geom_line(size = 1)+
  # Definindo os textos ao redor do gráfico
  labs(
    title = "Mortes confirmadas por Covid-19 (Estado de SP)",
    subtitle = "Total por FAIXA ETÁRIA e por SEMANA DO ÓBITO",
    caption = "Fonte: Sivep-Gripe (Ministério da Saúde)",
    y = "total de mortes",
    x = "semana"
  )+
  # Definindo as cores das linhas e os detalhes da legenda
  scale_color_manual(
    values=c("#9398B8", "#8BACBC", "#3D748F", "#a80000", "#7E0F08")
  )+
  geom_point()+
  # Definindo cores e formas dos elementos de fundo do gráfico
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white", colour="grey"),
    panel.grid.major = element_line(colour = "grey", linetype = "dotted"),
    panel.grid.minor = element_line(colour = "grey", linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.caption = element_text(face = "italic"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 8, face = "bold"),
    legend.background = element_rect(fill="#EEEEEE",
                                     size=0.5, linetype="solid")
  )+
  # salvando uma versão do gráfico em formato .SVG para ser facilmente replicado no Illustrator
  ggsave('docs/grafico_obitos_covid_sp_idade_20_sem.svg', dpi = 'retina',
         height = 8, width = 12, limitsize = F)


ggplot_obitos_covid_sp_idade_20_sem

