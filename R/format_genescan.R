#This script automates the formatting and initial processing of genescan data


#' This function takes a genescan file outputted from ThermoFishers Peakscanner application
#' as input. It assumes the user has converted the .csv to a .xlsx, but otherwise has not touched the file.
#' Required column names are 'Sample Filename', 'Dye Color', 'Size', and 'Height'. A period may be used
#' in place of whitespace. Outputs a .xlsx file containing one row for each sample, with columns for
#' the height and size of the top three peaks above a user-specified Size cutoff for a user specified Dye Colour.
#' Also contains a few extra, empty columns for downstream analysis our lab commonly uses
#' Recommend running with full file paths
#'
#' @param file Path to the input file
#' @param out Path for the output to be written to
#' @param sizeCutoff Numeric value below which peaks will be filtered from the dataset. Default is 200
#' @param dye Character value to select which colour dye will be used to extract peaks. Default is 'BLUE'
#' @returns A .xslx file containing the top 3 peaks for each sample and their ratio. One row per sample
#' @export
format_genescan <- function(file, out, sizeCutoff = 200, dye = 'BLUE'){
  
  #read in excel file
  data <- openxlsx::read.xlsx(file)
  
  #Remove all rows that don't contain BLUE dye
  data <- data[which(data$Dye.Color == dye),]
  
  #Remove all peaks below 200 size
  data <- data[which(as.numeric(data$Size) >= sizeCutoff),]
  
  ##Obtain three highest peaks for each sample
  #First sort the dataframe by Sample Filename
  #Then for each unique sample filename, get the first 3 rows with that name
  
  data <- data[with(data, order(Sample.Filename, -Height)),]
  
  data[which(data$Sample.Filename == unique(data$Sample.Filename)[1]),][1:3,]
  
  samples <- unique(data$Sample.Filename)
  indices <- c()
  for(i in 1:length(samples)){
    indices <- c(indices, which(data$Sample.Filename == samples[i])[1:3])
  }
  
  data <- data[indices,]
  
  #reformat table to get one row for each sample
  newdata <- as.data.frame(matrix(data = NA, nrow = length(samples), ncol = 12))
  colnames(newdata) <- c('Sample.Filename', 'Isolate', 'Primary.Height', 'Primary.Size', 'Secondary.Height', 
                         'Secondary.Size', 'Ratio', 'Tertiary.Height', 'Tertiary.Size', 'Genomic.prediction', 
                         'Genescan.prediction', 'Sequence.prediction')
  
  for(i in 1:length(samples)){
    newdata$Sample.Filename[i] <- samples[i]
    newdata$Primary.Height[i] <- data$Height[which(data$Sample.Filename == samples[i])][1]
    newdata$Primary.Size[i] <- data$Size[which(data$Sample.Filename == samples[i])][1]
    newdata$Secondary.Height[i] <- data$Height[which(data$Sample.Filename == samples[i])][2]
    newdata$Secondary.Size[i] <- data$Size[which(data$Sample.Filename == samples[i])][2]
    newdata$Tertiary.Height[i] <- data$Height[which(data$Sample.Filename == samples[i])][3]
    newdata$Tertiary.Size[i] <- data$Size[which(data$Sample.Filename == samples[i])][3]
    
    #Calculate ratio between primary and secondary
    newdata$Ratio[i] <- newdata$Primary.Height[i]/newdata$Secondary.Height[i]
  }
  
  openxlsx::write.xlsx(newdata, file = out)
}


format_genescan(infile, outputname, sizeCutoff = 200, dye = 'BLUE')


