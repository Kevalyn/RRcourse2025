library(quarto)

# Set working directory to where the .qmd file is
setwd("10. MD and Quarto 3")

for (s in 1:8) {
  quarto_render("Assignment_with_params.qmd",
                execute_params = list(
                  season = s,
                  data = paste0("../Data/season_", s, ".csv")
                ),
                output_file = paste0("GoT_Season_", s, ".html")
  )
}

