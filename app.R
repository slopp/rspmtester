library(shiny)
library(httr)
library(stringr)

pkgs <- available.packages(repos = "https://solo.rstudiopm.com/cran/latest")
base_url <- "https://solo.rstudiopm.com/cran/__linux__/%s/latest"

# --- helpers
package_type <- function(package, rver, distro, base_url){
  ver <- pkgs[which(pkgs[,"Package"] == package), "Version"]
  distro_id <- str_remove_all(str_to_lower(distro), " ")
  repo <- sprintf(base_url, distro_id)
  pkg_url <- sprintf("%s/src/contrib/%s_%s.tar.gz", repo, package, ver)
  res <- HEAD(pkg_url, user_agent(sprintf(" R (%s.0 x86_64-pc-linux-gnu x86_65 linux-gnu)", rver)))
  list(ver = ver,
       status = ifelse(res$status_code == 200, res$headers$`x-package-type`, "package-not-found")
  )
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
     textOutput("version"),
     tableOutput("results")
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
        res <- package_type(input$package, r, os, base_url)
        
        result <- data.frame(
          os = os,
          r_version = r,
          result = res$status,
          stringsAsFactors = FALSE
        )
        results <- rbind(results, result)
      }
    }
    list(results = results, ver = res$ver)
  })
  
  output$version <- renderText({
    data()$ver
  })
  
  output$results <- renderTable({
    data()$results
  })
}

shinyApp(ui, server)
