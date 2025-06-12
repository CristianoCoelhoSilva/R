library(ggplot2)
library(dplyr)

# Seus dados (substitua por sua base de dados real se for diferente)
dados <- validacao

# Garanta que a coluna 'Ano' seja numérica e então converta para fator.
# Isso é crucial para o geom_tile tratar cada ano como uma categoria distinta.
dados$Ano <- as.factor(as.character(dados$Ano))

# Gerar o gráfico de tiles (quadradinhos coloridos)
ggplot(dados, aes(x = Ano, y = 1, fill = media_temperatura_maxima)) +
  geom_tile(color = "white", linewidth = 0.5, width = 0.9) + # Aumenta o tamanho dos quadrados
  scale_fill_gradient(low = "lightblue", high = "red") + # Mapeia temperaturas baixas para azul e altas para vermelho
  labs(
    title = "",
    x = "Year",
    y = NULL, # Remove o rótulo do eixo Y
    fill = "Max. Average Temp. (°C)" # Título da legenda de cores (levemente encurtado)
  ) +
  theme_minimal() + # Um tema limpo para o gráfico
  scale_x_discrete(breaks = unique(dados$Ano),
                   guide = guide_axis(n.dodge = 2)) + # Opcional: para evitar sobreposição de rótulos em muitos anos
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title.x = element_text(size = 10), # Título do eixo X
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8), # Rótulos do eixo X
    axis.text.y = element_blank(), # Remove os rótulos do eixo Y (pois é apenas '1')
    axis.ticks.y = element_blank(), # Remove os ticks do eixo Y
    strip.text = element_text(size = 9, face = "bold"), # Rótulos dos subplots (nome das capitais)
    legend.position = "bottom", # Move a legenda de cor para baixo
    # --- AJUSTES NA LEGENDA AQUI ---
    legend.title = element_text(size = 8, face = "bold"), # Tamanho da fonte do título da legenda
    legend.text = element_text(size = 7),                 # Tamanho da fonte dos rótulos da legenda
    legend.key.width = unit(0.8, "cm"),                   # Largura da barra da legenda
    legend.key.height = unit(0.3, "cm"),                  # Altura da barra da legenda
    # --- FIM DOS AJUSTES NA LEGENDA ---
    panel.grid.major.y = element_blank(), # Remove as grades horizontais
    panel.grid.minor.y = element_blank() # Remove as grades horizontais menores
  ) +
  facet_wrap(~ DC_NOME, ncol = 3, scales = "fixed")