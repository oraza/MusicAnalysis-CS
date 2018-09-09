library(tidyverse)
library(stringr)
library(tuneR)
library(ggridges)
library(ggplot2)
library(knitr)
library(kableExtra)
library(magrittr)
library(dplyr)
library(viridis)

processSong <- function(mp3FilePath, widthSample = 4096) {
  stereoMP3File <- readMP3(mp3FilePath)
  wavFile <- extractWave(stereoMP3File, interact = FALSE)
  if (nchannel(wavFile) > 1) {
    wavFile <- mono(wavFile, "both")
  }
  perioWav <- periodogram(wavFile, width = widthSample)
  freqWav <- FF(perioWav)
  return(freqWav)
}

singersFreqRange <- readxl::read_xlsx("C:\\Users\\Owais\\Desktop\\mudatadetails.xlsx") %>%
  mutate(mp3FileAddress = str_c("C:\\Users\\Owais\\Desktop\\mudata\\", 
                                str_pad(row_number(), 2, pad = "0"), ".mp3"),
         Frequency = map(mp3FileAddress, processSong)) %>%
  unnest(Frequency)

singersFreqRange %>%
  ggplot(aes(x = Frequency, y = singer)) +
  geom_density_ridges()

singersFreqRange %>%
  select(singer, title, Frequency) %>%
  remove_missing(na.rm = TRUE) %>%
  arrange(Frequency) %>%
  slice(c(1:15, (n() - 2):n())) 

.breaks <-  c(49, 98, 196, 392, 783.99, 1567.98, 3135.96)
.labels <- c(expression("G"[1]), expression("G"[2]), expression("G"[3]),
             expression("G"[4]), expression("G"[5]), expression("G"[6]),
             expression("G"[7]))


singersFreqRange %>%
  group_by(singer) %>%
  mutate(minFreq = 20,
         maxFreq = quantile(Frequency, 0.99, na.rm = TRUE)) %>%
  filter(Frequency > minFreq, Frequency < maxFreq) %>%
  mutate(medianFreq = median(Frequency, na.rm = TRUE),
         maxFreq = max(Frequency, na.rm = TRUE)) %>%
  ggplot(aes(x = Frequency, y = reorder(singer, -medianFreq), fill = ..x..)) +
  geom_density_ridges_gradient(rel_min_height = 0.0, panel_scaling = F) +
  scale_fill_viridis(name = "Freq.[Hz]", option = "D" ) +
  theme_ridges(font_size = 13, grid = TRUE) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size=12),
        text = element_text(family = "mono")) +
  labs(title = "Nusrat Fateh Ali Khan's Vocal Range",
       subtitle = 'Frequency [Hz] Distribution (ordered by Median)') +
  scale_x_continuous(breaks = .breaks,
                     labels = .labels, limits = c(0, 1150))
