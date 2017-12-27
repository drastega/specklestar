library(tidyverse)
library(ff) # ff classes for big data
library(imager) # for visualization (eg. as.cimg())
library(rgl)

t0 <- Sys.time() # start time

#data_file <- '/Users/leda/home/Reduction/mavr/cyg_ob2_12_800.dat'
data_file <- file.choose()
N_frames <- file.info(data_file)$size/(512 * 512 * 2)
data <- ff(filename = data_file, readonly = TRUE, dim = c(512, 512, N_frames), vmode = 'ushort')

middle_frame <- matrix(0, 512, 512)
for (i in seq(N_frames)) {
  middle_frame <- middle_frame + data[, , i]
}

middle_frame <- middle_frame / N_frames

print(Sys.time() - t0) # runtime calculation

### Visualization

## imager
plot(as.cimg(middle_frame))
