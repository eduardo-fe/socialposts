# ======================================
# Escaños en Blanco Simulation - Full R Script
# ======================================

# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)       # For spatial maps
library(viridis)  # Color scales

# -------------------------------
# 1. Load Excel file
# -------------------------------
vote_data <- read_excel("Desktop/tweets/escanos/vote2023.xlsx")

# -------------------------------
# 2. Compute abstention
# -------------------------------
vote_data <- vote_data %>%
  mutate(
    Abstention = `Total censo electoral` - `Total votantes`
  )

# -------------------------------
# 3. Identify party vote columns
# -------------------------------
# This regex captures only the vote columns, not the Diputados columns
party_cols <- grep("^(PP|PSOE|Vox|SUMAR|ERC|JxCAT|EH Bildu|EAJ-PNV|B\\.N\\.G\\.|COALICIÓN CANARIA|UNION DEL PUEBLO NAVARRO|PACMA|CANDIDATURA D'UNITAT POPULAR|FRENTE OBRERO|NUEVA CANARIAS|PARTIT DEMÒCRATA EUROPEU|RECORTES CERO|POR UN MUNDO MÁS JUSTO|UNIÓN DEL PUEBLO LEONÉS|ARAGÓN EXISTE|PARTIDO COMUNISTA|GEROA BAI|SORIA ¡YA!|ADELANTE ANDALUCÍA|ESCAÑOS EN BLANCO|JAÉN MERECE MÁS|POR ÁVILA|BLOQUE EXTREMEÑO|CAMINANDO JUNTOS|FALANGE|PARTIDO ARAGONÉS|ESPAÑA VACIADA|PARTIDO HUMANISTA|ASTURIAS EXISTE EV|POR HUELVA|VAMOS PALENCIA|ZAMORA SÍ|VÍA BURGALESA|POR MI REGIÓN|AHORA CANARIAS|PARTIDO AUTÓNOMOS|ESTAT VALENCIÀ DEL BENESTAR|COALICIÓN POR MELILLA|JUNTOS POR GRANADA|ESPAÑA VACIADA|PARTIDO REGIONALISTA|SOMOS CÁCERES|ALMERIENSES|FEDERACIÓN DE LOS INDEPENDIENTES|TERCERA EDAD|UNIDAD CASTELLANA|GRUPO INDEPENDIENTE|PARTIDO UNIONISTA|ENTRE VEÏNS|LIBRES|UNIDOS POR LA SOLIDARIDAD|REFERÉNDUM|COALICIÓN DE CENTRO|FUERZA CÍVICA)$",
                   colnames(vote_data))

party_cols <- grep("^(PP|PSOE|Vox|SUMAR|ERC|JxCAT - JUNTS|EH Bildu|EAJ-PNV|B\\.N\\.G\\.|COALICIÓN CANARIA|UNION DEL PUEBLO NAVARRO|PACMA|CANDIDATURA D'UNITAT POPULAR|FRENTE OBRERO|NUEVA CANARIAS|PARTIT DEMÒCRATA EUROPEU|RECORTES CERO|POR UN MUNDO MÁS JUSTO|UNIÓN DEL PUEBLO LEONÉS|ARAGÓN EXISTE|PARTIDO COMUNISTA|GEROA BAI|SORIA ¡YA!|ADELANTE ANDALUCÍA|ESCAÑOS EN BLANCO|JAÉN MERECE MÁS|POR ÁVILA|BLOQUE EXTREMEÑO|CAMINANDO JUNTOS|FALANGE|PARTIDO ARAGONÉS|ESPAÑA VACIADA|PARTIDO HUMANISTA|ASTURIAS EXISTE EV|POR HUELVA|VAMOS PALENCIA|ZAMORA SÍ|VÍA BURGALESA|POR MI REGIÓN|AHORA CANARIAS|PARTIDO AUTÓNOMOS|ESTAT VALENCIÀ DEL BENESTAR|COALICIÓN POR MELILLA|JUNTOS POR GRANADA|ESPAÑA VACIADA|PARTIDO REGIONALISTA|SOMOS CÁCERES|ALMERIENSES|FEDERACIÓN DE LOS INDEPENDIENTES|TERCERA EDAD|UNIDAD CASTELLANA|GRUPO INDEPENDIENTE|PARTIDO UNIONISTA|ENTRE VEÏNS|LIBRES|UNIDOS POR LA SOLIDARIDAD|REFERÉNDUM|COALICIÓN DE CENTRO|FUERZA CÍVICA)$", colnames(vote_data))


party_names <- colnames(vote_data)[party_cols]

# -------------------------------
# 4. Add Escaños en Blanco votes
# -------------------------------
add_escanos <- function(df, abst_frac = 0){
  df <- df %>%
    mutate(
      Escanos_en_Blanco = `Votos en blanco` + Abstention * abst_frac
    )
  # Add as last column
  df <- df %>%
    select(everything(), Escanos_en_Blanco)
  return(df)
}

abst_scenarios <- c(1)
df <- add_escanos(vote_data, abst_frac = abst_scenarios)

# -------------------------------
# 5. D'Hondt seat allocation
# -------------------------------
dhondt <- function(votes_vec, n_seats){
  n <- length(votes_vec)
  seats <- rep(0, n)
  quotients <- votes_vec
  for(i in 1:n_seats){
    winner <- which.max(quotients)
    seats[winner] <- seats[winner] + 1
    quotients[winner] <- votes_vec[winner] / (seats[winner] + 1)
  }
  return(seats)
}

library(purrr)  # for pmap

# -------------------------------
# 6. Compute Total Deputies per province
# -------------------------------
# Identify all Diputados columns
diputados_cols <- grep("^Diputados\\.\\.\\.", colnames(df))
df <- df %>%
  mutate(Total_Diputados = rowSums(select(., all_of(diputados_cols)), na.rm = TRUE))

# -------------------------------
# 7. Apply D'Hondt per province (row-wise)
# -------------------------------

# Extract votes matrix: all parties + Escaños en Blanco
votes_matrix <- df %>%
  select(all_of(party_cols), Escanos_en_Blanco)

# Apply dhondt function row-wise using pmap
dhondt_results <- pmap(
  list(asplit(as.matrix(votes_matrix), 1), df$Total_Diputados),
  dhondt
)

# Convert to a data frame
dhondt_df <- as.data.frame(do.call(rbind, dhondt_results))
colnames(dhondt_df) <- c(party_names, "Escanos_en_Blanco")

# Combine with province info
final_df <- cbind(
  df[, c("Nombre de Comunidad", "Nombre de Provincia", "Total_Diputados")],
  dhondt_df
)

# -------------------------------
# 8. Check results
# -------------------------------
head(final_df)






library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# -------------------------------
# 1. Aggregate seats nationally
# -------------------------------
national_seats <- final_df %>%
  select(-c("Nombre de Comunidad", "Nombre de Provincia", "Total_Diputados")) %>%
  summarise(across(everything(), sum)) %>%
  pivot_longer(cols = everything(), names_to = "Party", values_to = "Seats") %>%
  filter(Seats > 0)

# -------------------------------
# 2. Assign party colours
# -------------------------------
party_colors <- c(
  "PP" = "#1A75CF",         
  "PSOE" = "#E60026",       
  "Vox" = "#4CBB17",        
  "SUMAR" = "#FF66CC",      
  "ERC" = "#FF6600",        
  "JxCAT - JUNTS" = "#FFD700",
  "EH Bildu" = "#006633",   
  "EAJ-PNV" = "#006600",    
  "B.N.G." = "#339933",     
  "Escanos_en_Blanco" = "grey70"
)
national_seats$Color <- party_colors[national_seats$Party]
national_seats$Color[is.na(national_seats$Color)] <- "grey80"

# -------------------------------
# 3. Prepare data for semicircle plot
# -------------------------------
national_seats <- national_seats %>%
  arrange(desc(Seats)) %>%
  mutate(
    CumSeats = cumsum(Seats),
    Mid = CumSeats - Seats/2,
    Fraction = Seats / sum(Seats),
    Label = paste0(Seats)
  )

# -------------------------------
# 4. Plot semi-circle parliament
# -------------------------------


# Column plot with seat numbers on top
ggplot(national_seats, aes(x = Party, y = Seats, fill = Party)) +
  geom_col(width = 0.7, color = "white") +
  geom_text(aes(label = Seats), vjust = -0.5, size = 4) +
  scale_fill_manual(values = national_seats$Color) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "Simulated National Distribution of Escaños en Blanco & Parties",
    y = "Number of Seats",
    x = "Party"
  )











# ======================================
# Compare Escaños en Blanco attribution scenarios
# ======================================

abst_scenarios <- c(0, 0.25, 0.5, 1)

compute_national_seats <- function(abst_frac){
  
  df <- add_escanos(vote_data, abst_frac = abst_frac)
  
  # Total deputies
  diputados_cols <- grep("^Diputados\\.\\.\\.", colnames(df))
  df <- df %>%
    mutate(Total_Diputados = rowSums(select(., all_of(diputados_cols)), na.rm = TRUE))
  
  # Votes matrix
  votes_matrix <- df %>% select(all_of(party_cols), Escanos_en_Blanco)
  
  # D'Hondt per province
  dhondt_results <- pmap(
    list(asplit(as.matrix(votes_matrix), 1), df$Total_Diputados),
    dhondt
  )
  
  dhondt_df <- as.data.frame(do.call(rbind, dhondt_results))
  colnames(dhondt_df) <- c(party_names, "Escanos_en_Blanco")
  
  final_df <- cbind(
    df[, c("Nombre de Comunidad", "Nombre de Provincia", "Total_Diputados")],
    dhondt_df
  )
  
  # Aggregate nationally
  final_df %>%
    select(-c("Nombre de Comunidad", "Nombre de Provincia", "Total_Diputados")) %>%
    summarise(across(everything(), sum)) %>%
    pivot_longer(cols = everything(), names_to = "Party", values_to = "Seats") %>%
    mutate(Attribution = abst_frac) %>%
    filter(Seats > 0)
}

# Compute all scenarios
comparison_df <- bind_rows(lapply(abst_scenarios, compute_national_seats))

# Add colors
comparison_df$Color <- party_colors[comparison_df$Party]
comparison_df$Color[is.na(comparison_df$Color)] <- "grey80"

# Plot comparison
ggplot(comparison_df,
       aes(x = Party, y = Seats, fill = Party)) +
  geom_col(width = 0.7, color = "white") +
  geom_text(aes(label = Seats), vjust = -0.4, size = 3) +
  scale_fill_manual(values = party_colors) +
  facet_wrap(~ Attribution, nrow = 1,
             labeller = labeller(Attribution = function(x)
               paste0("Abstention attribution = ", x))) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "National Seat Allocation under Escaños en Blanco Attribution Scenarios",
    x = "Party",
    y = "Number of Seats"
  )






# ======================================
# Heatmap: National seats by attribution
# (Fixed Escaños en Blanco + softer colors)
# ======================================

library(dplyr)
library(ggplot2)
library(viridis)

party_colors <- c(
  "PP" = "#1A75CF",          # People's Party (sky blue)
  "PSOE" = "#E60026",        # Spanish Socialist Workers’ Party (red)
  "Vox" = "#4CBB17",         # Vox (green)
  "SUMAR" = "#EC207A",       # Sumar (magenta/pink)
  "ERC" = "#FF6600",         # Esquerra Republicana (orange)
  "JxCAT - JUNTS" = "#30C4C8", # Junts / JxCat (turquoise)
  "EH Bildu" = "#00AC8E",    # EH Bildu (teal/green)
  "EAJ-PNV" = "#006600",     # Basque Nationalist Party (green)
  "B.N.G." = "#339933",      # Bloque Nacionalista Galego (green)
  "Escanos_en_Blanco" = "yellow"
)

# Ensure Escanos_en_Blanco exists at all attribution levels
comparison_df <- comparison_df %>%
  complete(Party, Attribution, fill = list(Seats = 0))

# Order parties by baseline seats (including Escanos_en_Blanco)
party_order <- comparison_df %>%
  filter(Attribution == 0) %>%
  arrange(desc(Seats)) %>%
  pull(Party)

comparison_df <- comparison_df %>%
  mutate(
    Party = factor(Party, levels = party_order),
    Attribution = factor(Attribution, levels = c(0, 0.25, 0.5, 1))
  )

comparison_df <- comparison_df %>%
  mutate(
    Party = if_else(is.na(Party), "Escanos en Blanco", Party),
    
  )

comparison_df <- comparison_df %>%
  mutate(
    Party = if_else(is.na(Party), "Escanos_en_Blanco", Party),
    Party = factor(Party, levels = party_order),
    Attribution = factor(Attribution, levels = c(0, 0.25, 0.5, 1)),
    PartyColor = party_colors[as.character(Party)]
  )




ggplot(comparison_df,
       aes(x = Attribution, y = Party, fill = Seats)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(
    aes(label = Seats, color = PartyColor),
    size = 3.2,
    fontface = "bold"
  ) +
  scale_fill_viridis_c(
    option = "C",
    begin = 0.15,
    end = 0.85,
    name = "Seats"
  ) +
  scale_color_identity() +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    legend.position = "right"
  ) +
  labs(
    title = "National Seat Allocation by Abstention Attribution",
    subtitle = "Tile color = total seats · Number color = party",
    caption = "Escaños en Blanco shown in grey"
  )
# Heatmap
ggplot(comparison_df,
       aes(x = Attribution, y = Party, fill = Seats)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = Seats), size = 3, color = "black") +
  scale_fill_viridis_c(
    option = "C",
    begin = 0.15,   # soften low end
    end = 0.85,     # soften high end
    name = "Seats"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11)
  ) +
  labs(
    title = "National Seat Allocation by Abstention Attribution",
    subtitle = "Rows: parties · Columns: fraction of abstention assigned to Escaños en Blanco",
    caption = "Numbers show total seats after D’Hondt aggregation"
  )




party_colors <- c(
  "PP" = "#1A75CF",          # People's Party (sky blue)
  "PSOE" = "#E60026",        # Spanish Socialist Workers’ Party (red)
  "Vox" = "#4CBB17",         # Vox (green)
  "SUMAR" = "#EC207A",       # Sumar (magenta/pink)
  "ERC" = "#FF6600",         # Esquerra Republicana (orange)
  "JxCAT - JUNTS" = "#30C4C8", # Junts / JxCat (turquoise)
  "EH Bildu" = "#00AC8E",    # EH Bildu (teal/green)
  "EAJ-PNV" = "#006600",     # Basque Nationalist Party (green)
  "B.N.G." = "#339933",      # Bloque Nacionalista Galego (green)
  "Escanos_en_Blanco" = "orange"
)


comparison_df <- comparison_df %>%
  mutate(Seats_alpha = log(Seats))

ggplot(comparison_df,
       aes(x = Attribution, y = Party, fill = Party)) +
  geom_tile(
    aes(alpha = Seats_alpha),
    color = "white",
    linewidth = 0.4
  ) +
  geom_text(
    aes(label = Seats),
    color = "black",
    size = 4.2,
    fontface = "bold"
  ) +
  scale_fill_manual(values = party_colors) +
  scale_alpha(range = c(0.2, 1), guide = "none") +
  scale_x_discrete(
    position = "top",
    labels = c(
      "0" = "0%",
      "0.25" = "25%",
      "0.5" = "50%",
      "1" = "100%"
    )
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 11),
    legend.position = "none"
  ) +
  labs(
    title = "National Seat Allocation by Abstention Attribution",
    subtitle = "Tile colour = party · Opacity ∝ √(seats)",
    caption = "Darker tiles indicate more seats (nonlinear scaling)"
  )


