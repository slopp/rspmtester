library(shiny)
library(httr)
library(gt)
library(stringr)

pkgs <- available.packages(repos = "https://demo.rstudiopm.com/cran/latest")
base_url <- "https://solo.rstudiopm.com/cran/__linux__/%s/latest"

# --- helpers
package_type <- function(package, rver, distro, base_url){
  ver <- pkgs[which(pkgs[,"Package"] == package), "Version"]
  distro_id <- str_remove_all(str_to_lower(distro), " ")
  repo <- sprintf(base_url, distro_id)
  pkg_url <- sprintf("%s/src/contrib/%s_%s.tar.gz", repo, package, ver)
  res <- HEAD(pkg_url, user_agent(sprintf(" R (%s.0 x86_64-pc-linux-gnu x86_65 linux-gnu)", rver)))
  ifelse(res$status_code == 200, res$headers$`x-package-type`, "package-not-found")
}

ui <- fluidPage(
  titlePanel("Binary Checker"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput('package', label = "Package to Check", choices = pkgs[,"Package"]),
      checkboxGroupInput('distros', label = "Distributions to Check", choices = c("Xenial", "Bionic", "CentOS 6", "CentOS 7", 
                                                                                  "OpenSUSE 15", "OpenSUSE 42"), selected = TRUE),
      checkboxGroupInput('rvers', label = "R Versions to Check", choices = c("3.4", "3.5", "3.6"), selected = TRUE)
    ),
    mainPanel(
     gt_output("results")
    )
  )
)

server <- function(input, output, session){
  
  data <- reactive({
    req(input$package)
    results <- data.frame(
      os = vector(mode = "character"),
      r_version = vector(mode = "character"),
      result = vector(mode = "character"),
      stringsAsFactors = FALSE
    )
    for(os in input$distros){
      for(r in input$rvers){
        result <- data.frame(
          os = os,
          r_version = r,
          result = package_type(input$package, r, os, base_url),
          stringsAsFactors = FALSE
        )
        results <- rbind(results, result)
      }
    }
    results
  })
  
  output$results <- render_gt({
    gt(data())
  })
}

shinyApp(ui, server)
