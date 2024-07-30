library(mzR)
library(tidyverse)

mzML_LTR <- mzR::openMSfile(filename = "/Users/lukegraywhiley/Library/CloudStorage/OneDrive-MurdochUniversity/projects/NICHOLSON-COVID/analysis/COVID_BIOGUNE/sciex_lipidomics/processing_lipidomics_biogune/mzml_files/covid19_biogune_SER_MS-LIPIDS_PLIP01_COVp20_160421-PLASMA LTR_30.mzML")

mzML_chrom <- mzR::chromatogram(mzML_LTR)

plot <- ggplot(mzML_chrom[[1]])

for(idx in 10:length(mzML_chrom)){
spline.d <- as.data.frame(spline(mzML_chrom[[idx]]))

plot <- plot+
  geom_line(data = spline.d, aes(x = x, y = y))
}


plot <- ggplot(mzML_chrom[[250]])
  spline.d <- as.data.frame(spline(mzML_chrom[[250]]))

plot <- plot+
  geom_line(data = spline.d, aes(x = x, y = y))
plot  

alsace::findpeaks(y = spline.d$y)
alsace::fitpeaks(y = spline.d$y, pos = 56)
