library(tools)
library(magrittr)

# SPE to dat conversion (i.e. removing 4100 byte SPE header)
spe2dat <- function(){
  input_file <- file.choose() # choose .SPE file
  output_file <- file_path_sans_ext(input_file) %>% paste(., 'dat', sep = '.')
  system(sprintf("dd if=%s of=%s bs=4100 skip=1", input_file, output_file))
  print(output_file)
}

# Take N frames from series of speckle images 512x512x2(bytes)
n_frames <- function(n, start = 1){
  input_file <- file.choose() # choose .dat file
  output_file <- file_path_sans_ext(input_file) %>% paste(., '_', as.character(n), '_frames.dat', sep = '')
  system(sprintf("dd if=%s of=%s bs=524288 count=%d skip=%d", input_file, output_file, n, (start - 1)))
  print(output_file)
}
