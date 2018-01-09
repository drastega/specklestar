library(ff) # ff classes for big data

middle_frame <- function(data_file) {
#data_file <- '/Users/leda/home/Reduction/mavr/cyg_ob2_12_800.dat'
#data_file <- file.choose()
N_frames <- file.info(data_file)$size/(512 * 512 * 2)
data <- ff(filename = data_file, readonly = TRUE, dim = c(512, 512, N_frames), vmode = 'ushort')

middle_frame <- matrix(0, 512, 512)
for (i in seq(N_frames)) {
  middle_frame <- middle_frame + data[, , i]
}

middle_frame <- middle_frame / N_frames
}
