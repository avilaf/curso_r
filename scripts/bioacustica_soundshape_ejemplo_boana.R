
# Sound Waves Onto Morphometric Data ######################################
# Example worksheet by Fernanda Rodrigues de Avila
# R version 4.5.0
# Contato: fernandar.avila@gmail.com
# https://avilaf.github.io/


# Packages: ---------------------------------------------------------------


# install.packages("SoundShape")
library(SoundShape)


# Set Working Directory:  -------------------------------------------------


# path <- "camino/para/datos/vocalizaciones"
setwd(path)
dir()

# Indicate where are stored the ".wav" files

wav_at <- path


# Create subfolder to store results

store_at <- file.path(getwd(), "output")

dir.create(store_at)



# Import and visualize data: ----------------------------------------------



## Boana semilineata -------------------------------------------------------
# 1- Import:
boana_semilineata <- tuneR::readWave("boana_semilineata.wav")
boana_semilineata

# 2- Plot:

seewave::spectro(boana_semilineata,
                 flim = c(0,6),
                 wl = 512,
                 f = 44100,
                 ovlp = 70,
                 grid = FALSE)




## Boana semilineata 2 -----------------------------------------------------
# 1- Import:
boana_semilineata_2 <- tuneR::readWave("boana_semilineata_2.wav")
boana_semilineata_2

# 2- Plot:
seewave::spectro(boana_semilineata_2,
                 flim = c(0,6),
                 wl = 512,
                 f = 44100,
                 ovlp = 70,
                 grid = FALSE)



## Boana pardalis ----------------------------------------------------------
# 1- Import:
boana_pardalis <- tuneR::readWave("boana_pardalis.wav")
boana_pardalis

# 2- Plot:
seewave::spectro(boana_pardalis,
                 flim = c(0, 1.8),
                 wl = 512,
                 f = 44100,
                 ovlp = 70,
                 grid = FALSE)


## Boana pardalis 2 --------------------------------------------------------
# 1- Import:
boana_pardalis_2 <- tuneR::readWave("boana_pardalis_2.wav")
boana_pardalis_2

# 2- Plot:
seewave::spectro(boana_pardalis_2,
                 flim = c(0, 1.8),
                 wl = 512,
                 f = 44100,
                 ovlp = 70,
                 grid = FALSE)


## Boana marginata ---------------------------------------------------------
# 1- Import:
boana_marginata <- tuneR::readWave("boana_marginata.wav")
boana_marginata


# 2- Plot:
seewave::spectro(boana_marginata,
                 flim = c(0, 4),
                 wl = 512,
                 f = 44100,
                 ovlp = 70,
                 grid = FALSE)


## Boana marginata 2 -------------------------------------------------------
# 1- Import:
boana_marginata_2 <- tuneR::readWave("boana_marginata_2.wav")
boana_marginata_2


# 2- Plot:
seewave::spectro(boana_marginata_2,
                 flim = c(0, 4),
                 wl = 512,
                 f = 44100,
                 ovlp = 70,
                 grid = FALSE)





## Boana faber -------------------------------------------------------------
# 1- Import:
boana_faber <- tuneR::readWave("boana_faber.wav")
boana_faber


# 2- Plot:
seewave::spectro(boana_faber,
                 flim = c(0, 1.8),
                 wl = 512,
                 f = 44100,
                 ovlp = 70,
                 grid = FALSE)



## Boana faber 2 ----------------------------------------------------------
# 1- Import:
boana_faber_2 <- tuneR::readWave("boana_faber_2.wav")
boana_faber_2




# 2- Plot:
seewave::spectro(boana_faber_2,
                 flim = c(0, 1.8),
                 wl = 512,
                 f = 44100,
                 ovlp = 70,
                 grid = FALSE)




# Align: -----------------------------------------------------------------


# Place sounds at the beginning of a sound window

SoundShape::align.wave(wav.at = wav_at,
                       wav.to = "Aligned",
                       time.length = .15,
                       flim = c(.1,5)
                       )

# Verify alignment using analysis.type = "twoDshape"


SoundShape::eigensound(analysis.type = "twoDshape",
                       wav.at = file.path(wav_at, "Aligned"),
                       store.at = store_at,
                       plot.exp = TRUE,
                       flim = c(0, 6),
                       tlim=c(0, .3)
                       )


# Eigenvectors: -----------------------------------------------------------



eig_boana <- SoundShape::eigensound(analysis.type = "threeDshape",
                                    wav.at = file.path(wav_at, "Aligned"),
                                    store.at = store_at,
                                    plot.exp = TRUE, 
                                    flim = c(0, 6), 
                                    tlim=c(0, .3)
                                    )


eig_boana




# PCA: --------------------------------------------------------------------

# PCA using three-dimensional semilandmark coordinates embeeded in eig.sample


pca_eig_boana <- stats::prcomp(geomorph::two.d.array(eig_boana))

# View summary results

summary(pca_eig_boana)

dimnames(eig_boana)[[3]]

sample_gr <- factor(c("faber",
                      "faber",
                      "marginata",
                      "marginata",
                      "pardalis",
                      "pardalis",
                      "semilineata",
                      "semilineata" ))


par(mfrow = c(1,1),
    mar=c(4,4,1,1))

plot <- SoundShape::pca.plot(pca_eig_boana,
                             groups = sample_gr,
                             conv.hulls = sample_gr,
                             leg.pos = "bottom",
                             cex = 1.2)



# Hypothetical sound surfaces: --------------------------------------------



# Mean shape configuration (consensus)



SoundShape::hypo.surf(eig_boana,
                      PC = "mean",
                      flim = c(0, 4),
                      tlim = c(0, 0.8),
                      x.length = 70,
                      y.length = 47,
                      cex.lab = 0.7,
                      cex.axis = 0.5,
                      cex.main = 1)



# Minimum and maximum deformations - Principal Component 1

SoundShape::hypo.surf(eig_boana,
                      PC = 1,
                      flim = c(0, 4),
                      tlim = c(0, 0.8),
                      x.length = 70,
                      y.length = 47,
                      cex.lab = 0.7,
                      cex.axis = 0.5,
                      cex.main = 1)

# Minimum and maximum deformations - Principal Component 2

SoundShape::hypo.surf(eig_boana,
                      PC = 2,
                      flim = c(0, 4),
                      tlim = c(0, 0.8),
                      x.length = 70,
                      y.length = 47,
                      cex.lab = 0.7,
                      cex.axis = 0.5,
                      cex.main = 1)




# Refferences:  -----------------------------------------------------------


citation("SoundShape")
citation()
RStudio.Version()
