# Curso R :)
# Ejemplo Tarea con datos

# Dra. Fernanda Rodrigues de Avila 
# <https://avilaf.github.io/>
# fernandar.avila@gmail.com

# Fuente de datos:
# https://avilaf.github.io/meus_artigos/Diet%20of%20Odontophrynus%20americanus%20in%20southern%20Atlantic%20Forest%20of%20Brazi.pdf



## 1. ANÁLISES EXPLORATÓRIAS INICIAIS

### 1.1 Estatísticas Descritivas

# Carregar bibliotecas necessárias
library(tidyverse)
library(vegan)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(RColorBrewer)

# Carregar dados
dados <- read.csv("https://raw.githubusercontent.com/avilaf/curso_r/refs/heads/main/datos/dados_odonto.csv")

# Resumo das variáveis por população
summary_stats <- dados %>%
  group_by(pop) %>%
  summarise_all(list(mean = mean, sd = sd, median = median), na.rm = TRUE)

# Visualizar estrutura dos dados
str(dados)
head(dados)
# 1.2 Preparação dos Dados
# Separar variáveis de volume e número
volume_cols <- c("gastropoda_v", "araneae_v", "coleoptera_v", "hymenoptera_v", 
                 "orthoptera_v", "lepidoptera_larva_v", "isopoda_v", "chilopoda_v", 
                 "diplopoda_v", "lepidoptera_v", "anelida_v", "dermaptera_v", 
                 "blattodea_v", "neoroptera_v")

numero_cols <- c("gastropoda_n", "araneae_n", "coleoptera_n", "hymenoptera_n", 
                 "orthoptera_n", "lepidoptera_larva_n", "isopoda_n", "chilopoda_n", 
                 "diplopoda_n", "lepidoptera_n", "anelida_n", "dermaptera_n", 
                 "blattodea_n", "neoroptera_n")

# Criar matriz de composição da dieta (volume)
matriz_volume <- dados[, volume_cols]
matriz_numero <- dados[, numero_cols]

# Adicionar identificação da população
matriz_volume$pop <- dados$pop
matriz_numero$pop <- dados$pop

# 2. GRÁFICOS DE COMPOSIÇÃO DA DIETA
# 2.1 Gráfico de Barras - Composição Média da Dieta
# Preparar dados para gráfico de barras
dieta_resumo <- dados %>%
  select(pop, all_of(volume_cols)) %>%
  group_by(pop) %>%
  summarise_all(mean, na.rm = TRUE) %>%
  pivot_longer(-pop, names_to = "item_alimentar", values_to = "volume_medio") %>%
  mutate(item_alimentar = str_remove(item_alimentar, "_v"))

# Gráfico de barras
p1 <- ggplot(dieta_resumo, aes(x = item_alimentar, 
                               y = volume_medio, 
                               fill = pop)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Composição Média da Dieta por Volume",
       x = "Item Alimentar", y = "Volume Médio (mm³)",
       fill = "População") +
  scale_fill_brewer(type = "qual", palette = "Set2")

p1

# 2.2 Gráfico de Pizza/Donut - Proporção da Dieta
# Calcular proporções por população

prop_dieta <- dados %>%
  select(pop, all_of(volume_cols)) %>%
  group_by(pop) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  pivot_longer(-pop, names_to = "item", values_to = "volume") %>%
  group_by(pop) %>%
  mutate(proporcao = volume / sum(volume),
         item = str_remove(item, "_v")) %>%
  filter(volume > 0)  # Remover itens não consumidos

# Gráfico de pizza para cada população
p2 <- ggplot(prop_dieta, aes(x = "", y = proporcao, fill = item)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  facet_wrap(~pop, labeller = labeller(pop = c("A" = "População A", "B" = "População B"))) +
  theme_void() +
  labs(title = "Proporção dos Itens Alimentares por População",
       fill = "Item Alimentar")

p2

# 3. ANÁLISES DE DIVERSIDADE
# 3.1 Índices de Diversidade
# Calcular índices de diversidade

diversidade_shannon <- dados %>%
  select(pop, all_of(volume_cols)) %>%
  group_by(pop) %>%
  group_modify(~ {
    matriz <- .x[, -1]
    shannon <- diversity(matriz, index = "shannon", MARGIN = 1)
    simpson <- diversity(matriz, index = "simpson", MARGIN = 1)
    riqueza <- specnumber(matriz, MARGIN = 1)
    data.frame(
      shannon = mean(shannon, na.rm = TRUE),
      simpson = mean(simpson, na.rm = TRUE),
      riqueza = mean(riqueza, na.rm = TRUE)
    )
  })

# Gráfico de diversidade
div_long <- diversidade_shannon %>%
  pivot_longer(-pop, names_to = "indice", values_to = "valor")

p3 <- ggplot(div_long, aes(x = pop, y = valor, fill = pop)) +
  geom_bar(stat = "identity") +
  facet_wrap(~indice, scales = "free_y", 
             labeller = labeller(indice = c("shannon" = "Diversidade Shannon",
                                            "simpson" = "Diversidade Simpson",
                                            "riqueza" = "Riqueza de Itens"))) +
  theme_minimal() +
  labs(title = "Índices de Diversidade por População",
       x = "População", y = "Valor do Índice", fill = "População") +
  scale_fill_brewer(type = "qual", palette = "Set2")

p3

# 3.2 Curvas de Rarefação
# Curvas de rarefação

# install.packages("iNEXT")
library(iNEXT)

# Preparar dados para iNEXT (removendo indivíduos com estômagos vazios)
lista_comunidades <- list()

for(pop in unique(dados$pop)) {
  subset_pop <- dados[dados$pop == pop, volume_cols]
  # Remover indivíduos com estômagos vazios
  subset_pop <- subset_pop[rowSums(subset_pop, na.rm = TRUE) > 0, ]
  if(nrow(subset_pop) > 0) {
    lista_comunidades[[paste("População", pop)]] <- colSums(subset_pop, na.rm = TRUE)
  }
}

# Gerar curvas de rarefação
rarefacao <- iNEXT(lista_comunidades, q = 0, datatype = "abundance")

p_rarefacao <- ggiNEXT(rarefacao) + 
  theme_minimal() +
  labs(title = "Curvas de Rarefação - Riqueza de Itens Alimentares",
       x = "Tamanho da Amostra", y = "Riqueza de Itens")


# 4. ANÁLISES MULTIVARIADAS
# 4.1 PCA (Análise de Componentes Principais)
# PCA baseado em volume (removendo indivíduos com estômagos vazios)

# Filtrar dados com estômagos não vazios
dados_filtrados <- dados[dados$vol_total > 0, ]

# Criar matriz de composição por volume
pca_dados <- dados_filtrados[, volume_cols]

# Substituir NA por 0, somar 1 e aplicar log
pca_dados[is.na(pca_dados)] <- 0
pca_dados <- pca_dados + 1
pca_dados_log <- log(pca_dados)

# Filtrar linhas com soma zero e sem NA
linhas_validas <- complete.cases(pca_dados_log) & rowSums(pca_dados_log, na.rm = TRUE) > 0
dados_final <- dados_filtrados[linhas_validas, ]
pca_dados_final <- pca_dados_log[linhas_validas, ]

# 4.1 PCA
pca_resultado <- prcomp(pca_dados_final, scale. = F, center = TRUE)

# DataFrame para gráfico
pca_df <- data.frame(pca_resultado$x[, 1:2],
                     pop = dados_final$pop,
                     vol_total = dados_final$vol_total)

# Gráfico PCA
p4 <- ggplot(pca_df, aes(x = PC1, 
                         y = PC2, 
                         color = pop, 
                         size = vol_total)) +
  geom_point(alpha = 0.7) +
  stat_ellipse(aes(fill = pop), 
               alpha = 0.2, 
               geom = "polygon", 
               show.legend = FALSE) +
  theme_minimal() +
  labs(
    title = "PCA - Composição da Dieta (dados log-transformados)",
    x = paste0("PC1 (", round(summary(pca_resultado)$importance[2, 1] * 100, 1), "%)"),
    y = paste0("PC2 (", round(summary(pca_resultado)$importance[2, 2] * 100, 1), "%)"),
    color = "População", size = "Volume Total"
  ) +
  scale_color_brewer(type = "qual", palette = "Set2") +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  scale_size_continuous(range = c(2, 6))


p4

# 4.2 NMDS
nmds_resultado <- metaMDS(pca_dados_final, distance = "bray", k = 2, trymax = 100)

# DataFrame para gráfico NMDS
nmds_df <- data.frame(
  NMDS1 = nmds_resultado$points[, 1],
  NMDS2 = nmds_resultado$points[, 2],
  pop = dados_final$pop,
  vol_total = dados_final$vol_total
)

# Gráfico NMDS
p5 <- ggplot(nmds_df, aes(x = NMDS1, y = NMDS2, color = pop, size = vol_total)) +
  geom_point(alpha = 0.7) +
  stat_ellipse(aes(fill = pop), alpha = 0.2, geom = "polygon", show.legend = FALSE) +
  theme_minimal() +
  labs(
    title = paste("NMDS - Stress:", round(nmds_resultado$stress, 3)),
    color = "População", size = "Volume Total"
  ) +
  scale_color_brewer(type = "qual", palette = "Set2") +
  scale_size_continuous(range = c(2, 6))

p5

# 5. ANÁLISES ESTATÍSTICAS

# 5.1 PERMANOVA
permanova_resultado <- adonis2(pca_dados_final ~ pop, data = dados_final, 
                               method = "bray", permutations = 999)
print(permanova_resultado)

# 5.2 Teste de Homogeneidade de Dispersões (betadisper)
dist_bray <- vegdist(pca_dados_final, method = "bray")
betadisper_resultado <- betadisper(dist_bray, dados_final$pop)
permutest_resultado <- permutest(betadisper_resultado)
print(permutest_resultado)

# 5.3 ANOSIM
anosim_resultado <- anosim(pca_dados_final, dados_final$pop, distance = "bray")
print(anosim_resultado)


# 6. GRÁFICOS COMPARATIVOS ESPECÍFICOS
# 6.1 Boxplots dos Principais Itens Alimentares
# Identificar itens mais abundantes (por volume total)

itens_principais <- dados %>%
  select(all_of(volume_cols)) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  pivot_longer(everything(), names_to = "item", values_to = "total") %>%
  arrange(desc(total)) %>%
  slice_head(n = 8) %>%  # Top 8 itens
  pull(item)

# Boxplots dos principais itens
dados_long <- dados %>%
  select(pop, all_of(itens_principais)) %>%
  pivot_longer(-pop, names_to = "item", values_to = "volume") %>%
  mutate(item = str_remove(item, "_v"),
         item = str_replace_all(item, "_", " "),
         item = str_to_title(item))

p6 <- ggplot(dados_long, aes(x = pop, y = volume, fill = pop)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  facet_wrap(~item, scales = "free_y", ncol = 2) +
  theme_minimal() +
  labs(title = "Distribuição dos Principais Itens Alimentares",
       x = "População", y = "Volume (mm³)", fill = "População") +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  theme(legend.position = "bottom")

p6

# 6.2 Correlação entre Volume e Número
# Correlação volume vs número para cada item

correlacoes <- data.frame()
for(i in 1:length(volume_cols)) {
  vol_col <- volume_cols[i]
  num_col <- numero_cols[i]
  
  # Filtrar dados onde ambos volume e número > 0
  dados_item <- dados[dados[[vol_col]] > 0 & dados[[num_col]] > 0, ]
  
  if(nrow(dados_item) > 3) {  # Mínimo de observações para correlação
    cor_total <- cor(dados_item[[vol_col]], dados_item[[num_col]], use = "complete.obs")
    
    # Correlações por população
    dados_A <- dados_item[dados_item$pop == "A", ]
    dados_B <- dados_item[dados_item$pop == "B", ]
    
    cor_A <- if(nrow(dados_A) > 2) cor(dados_A[[vol_col]], dados_A[[num_col]], use = "complete.obs") else NA
    cor_B <- if(nrow(dados_B) > 2) cor(dados_B[[vol_col]], dados_B[[num_col]], use = "complete.obs") else NA
    
    correlacoes <- rbind(correlacoes, data.frame(
      item = gsub("_v$", "", vol_col),
      cor_total = cor_total,
      cor_A = cor_A,
      cor_B = cor_B,
      n_obs = nrow(dados_item)
    ))
  }
}

# Gráfico de correlações (apenas itens com correlações válidas)
if(nrow(correlacoes) > 0) {
  cor_long <- correlacoes %>%
    filter(!is.na(cor_total)) %>%
    pivot_longer(c(cor_total, cor_A, cor_B), names_to = "grupo", values_to = "correlacao") %>%
    mutate(grupo = recode(grupo, 
                          "cor_total" = "Total",
                          "cor_A" = "População A", 
                          "cor_B" = "População B"),
           item = str_replace_all(item, "_", " "),
           item = str_to_title(item)) %>%
    filter(!is.na(correlacao))
  
  p7 <- ggplot(cor_long, aes(x = reorder(item, correlacao), y = correlacao, fill = grupo)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    theme_minimal() +
    labs(title = "Correlação Volume vs Número por Item Alimentar",
         x = "Item Alimentar", y = "Correlação (r)", fill = "Grupo") +
    scale_fill_brewer(type = "qual", palette = "Set2") +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5)
}

p7

# 7. PAINEL DE GRÁFICOS FINAL
# Combinar gráficos principais

grid.arrange(p1, p3, p4, p7, ncol = 2, nrow = 2,
             top = "Análise Comparativa da Dieta entre Populações A e B")

# Salvar resultados
ggsave("analise_dieta_anfibios.png", width = 16, height = 12, dpi = 300)

# 8. Referências:

citation()

RStudio.Version()

citation(tidyverse)
citation(vegan)
citation(ggplot2)
citation(gridExtra)
citation(corrplot)
citation(RColorBrewer)

# R version 4.5.0 (2025-04-11 ucrt)

# Dra. Fernanda Rodrigues de Avila 
# <https://avilaf.github.io/>
# fernandar.avila@gmail.com

