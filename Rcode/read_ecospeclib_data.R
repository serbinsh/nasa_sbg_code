####################################################################################################
#
#  	--- Last updated:  07.01.2021 BY Shawn Serbin <sserbin@bnl.gov>
####################################################################################################


#--------------------------------------------------------------------------------------------------#
# Close all devices and delete all variables.
rm(list=ls(all=TRUE))   # clear workspace
graphics.off()          # close any open graphics
closeAllConnections()   # close any open connections to files
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Load libraries
list.of.packages <- c("dplyr","here","ggplot2","gridExtra","reshape2")
invisible(lapply(list.of.packages, library, character.only = TRUE))

here::here()
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Setup options
gather_spectra <- FALSE #TRUE/FALSE

spec_file_type <- "spectrum"
spec_wave_rng <- "vswir"
spec_ext <- "txt"

# Set output directory
output_dir <- file.path(here::here(),'Routput','ecospeclib')
if (! file.exists(output_dir)) dir.create(file.path(output_dir),recursive=TRUE, showWarnings = FALSE)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### gather raw data
spectra_root_dir <- file.path(here::here(),'data','ecospeclib-1625150308528')

if (gather_spectra) {
  
  # Create list of spectra folders
  full_spectrum_file_list <- list.files(spectra_root_dir, paste0(spec_wave_rng,"*.*",spec_file_type,
                                                                 "*.*",".",spec_ext,'$'), 
                                        recursive = TRUE)
  
  # Grab all results
  type <- lapply(file.path(spectra_root_dir, full_spectrum_file_list), read.csv, header = FALSE,
                 skip=1, nrows=1) %>% bind_rows(.id = 'file_number')
  type$Type <- gsub("Type:", "", type$V1)
  type$Type <- sub("\\s+", "", type$Type)
  type <- type[,-2]
  
  spec_class <- lapply(file.path(spectra_root_dir, full_spectrum_file_list), read.csv, header = FALSE,
                       skip=2, nrows=1) %>% bind_rows(.id = 'file_number')
  spec_class$Class <- gsub("Class:", "", spec_class$V1)
  spec_class$Class <- sub("\\s+", "", spec_class$Class)
  spec_class <- spec_class[,-2]
  
  genus <- lapply(file.path(spectra_root_dir, full_spectrum_file_list), read.csv, header = FALSE,
                 skip=3, nrows=1) %>% bind_rows(.id = 'file_number')
  genus$Genus <- gsub("Genus:", "", genus$V1)
  genus$Genus <- sub("\\s+", "", genus$Genus)
  genus <- genus[,-2]
  head(genus)

  spp <- lapply(file.path(spectra_root_dir, full_spectrum_file_list), read.csv, header = FALSE,
                  skip=4, nrows=1) %>% bind_rows(.id = 'file_number')
  spp$Species <- gsub("Species:", "", spp$V1)
  spp$Species <- sub("\\s+", "", spp$Species)
  spp <- spp[,-2]
  
  samp_num <- lapply(file.path(spectra_root_dir, full_spectrum_file_list), read.csv, header = FALSE,
                skip=5, nrows=1) %>% bind_rows(.id = 'file_number')
  samp_num$Samp_Num <- gsub("Sample No.:", "", samp_num$V1)
  samp_num$Samp_Num <- sub("\\s+", "", samp_num$Samp_Num)
  samp_num <- samp_num[,-2]

  spectra_data.1 <- lapply(file.path(spectra_root_dir, full_spectrum_file_list), read.csv, 
                           header = FALSE, skip=20) %>% bind_rows(.id = 'file_number')
  head(spectra_data.1)[1:2,]
  #spectra_data.2 <- data.frame(transform(spectra_data.1, reshape2::colsplit(V1, pattern = "\\\t", 
  #                                                                    names = c('Wave', 'Refl'))))
  spectra_data.2 <- data.frame(file_number=spectra_data.1[,1],reshape2::colsplit(spectra_data.1$V1, 
                                                                                 pattern = "\\\t",
                                                                                 names = c('Wave', 
                                                                                           'Refl')))
  head(spectra_data.2)[1:2,]
  
  spectra_data.3 <- spectra_data.2 %>%
    #select(file_number,Refl) %>%
    tidyr::pivot_wider(names_from=file_number,values_from=Refl)
  head(spectra_data.3)[1:2,1:10]
  
  # transpose into long format
  spectra_data.4 <- data.frame(t(spectra_data.3))
  head(spectra_data.4)[1:5,1:10]
  names(spectra_data.4) <- paste0("Wave_",seq(350,2500,1))
  spectra_data.4 <- spectra_data.4[-1,]
  spectra_data.5 <- data.frame(file_number=row.names(spectra_data.4), spectra_data.4)
  head(spectra_data.5)[1:5,1:10]

  output_df <- data.frame(file_number=spectra_data.5$file_number, Type=type[,2], 
                          Class=spec_class[,2], Genus=genus[,2], Species=spp[,2],
                          Sample_Number=samp_num[,2],
                          spectra_data.5[which(names(spectra_data.5) %in% paste0("Wave_",seq(350,2500,1)))])
  head(output_df)[1:5,1:10]
  
  # output parsed spectra
  write.csv(x = output_df[,-1], file = file.path(output_dir,"parsed_spectra.csv"),row.names = F)
  
  spectra_data <- output_df[,-1]
  head(spectra_data)[,1:10]
  
} else {
  
  ## get previously generated data file
  spectra_data <- read.csv(file = file.path(output_dir,"parsed_spectra.csv"), header = T)
  head(spectra_data)[,1:10]
}
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### create some plots
bark_spec <- spectra_data %>%
  filter(Class=="bark") %>%
  select(Genus,Species,starts_with("Wave_")) 

num_spec <- length(unique(bark_spec$Genus))

png(file = file.path(output_dir, "Mean_Bark_Spectra_by_Genus.png"),height=3000,
    width=4900, res=340)
par(mfrow=c(2,num_spec/2), mar=c(4.5,5.0,1.3,0.4), oma=c(0.3,0.9,0.3,0.1)) # B, L, T, R
for (i in unique(bark_spec$Genus)) {
  print(i)
  temp_spec <- bark_spec %>%
    filter(Genus==i) %>%
    summarise(across(Wave_400:Wave_2400, mean))
  plot(seq(400,2400,1), temp_spec[1,], type="l", lwd=3, main=paste0(i), 
       xlab="Wavelength (nm)", ylab="Reflectance (%)", cex.axis=1.4,
       cex.lab=2, cex.main=1.4)
  box(lwd=2.2)
}
dev.off()
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### EOF