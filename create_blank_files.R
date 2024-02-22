create_named_txt_files <- function(numbers) {
  # Check if the "data" folder exists; if not, create it
  if (!dir.exists("data")) {
    dir.create("data")
  }
  
  # Iterate through the numbers vector to create blank .txt files with specified names
  for (i in numbers) {
    file_name <- paste0("data/", i, ".txt") # Construct file name using the number
    file.create(file_name) # Create a blank .txt file
  }
  
  cat(paste0(length(numbers), " blank .txt files have been created in the 'data' folder with specified names.\n"))
}


