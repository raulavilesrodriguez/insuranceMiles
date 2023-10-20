library(config)
library(DBI)
library(lubridate)
library(glue)
library(leaflet)
library(shiny)
library(DT)
library(RPostgreSQL)
library(pool)
library(shinyjs)
library(uuid)
library(dplyr)
library(tidyverse)
library(shinydashboard)
library(shinyauthr)

#Read the database connection parameters from the config.yml
config_file <- "config.yml"
config <- config::get(file = config_file)

# Create a database connection
db <- dbPool(RPostgres::Postgres(),
                           host = config$host,
                           port = config$port,
                           dbname = config$dbname,
                           user = config$user,
                           password = config$password)

# a user who has not visited the app for this many days
# will be asked to login with user name and password again
cookie_expiry <- 7 # Days until session expires


# This function must accept two parameters: user and sessionid. It will be called whenever the user
# successfully logs in with a password.  This function saves to your database.

add_sessionid_to_db <- function(users, sessionid, conn = db) {
  tibble(users = users, sessionid = sessionid, login_time = as.character(now())) %>%
    dbWriteTable(conn, "sessionids", ., append = TRUE)
}

# This function must return a data.frame with columns user and sessionid  Other columns are also okay
# and will be made available to the app after log in as columns in credentials()$user_auth

get_sessionids_from_db <- function(conn = db, expiry = cookie_expiry) {
  dbReadTable(conn, "sessionids") %>%
    mutate(login_time = ymd_hms(login_time)) %>%
    as_tibble() %>%
    filter(login_time > now() - days(expiry))
}


# dataframe that holds usernames, passwords and other user data
user_base <- dbReadTable(db, "acceso") |> as_tibble()


# create the dataframe to 'titular' database. 
responses_df <- data.frame(row_id = character(),
                          nombre = character(),
                          sexo = character(),
                          edad = as.numeric(character()),
                          millas = as.numeric(character()),
                          cedula = character(),
                          email = character(),
                          comentario = character(),
                          creado = as.Date(character()),
                          stringsAsFactors = FALSE
                          )

# Create responses tabl in PostegreSQL
dbWriteTable(db, 
             "responses_df",
             responses_df,
             overwrite = FALSE,
             append = TRUE)

# to mark * any fields in the entry form that are mandatory
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <- ".mandatory_star { color: red; }"


#--------Shiny APP---------
ui <- dashboardPage(
  dashboardHeader(
    title = "Millas app",
    tags$li(
      class = "dropdown",
      style = "padding: 8px;",
      shinyauthr::logoutUI("logout") # add logout button UI
    ),
    tags$li(
      class = "dropdown",
      tags$a(
        icon("github"),
        href = "https://raulaviles.netlify.app/",
        title = "Autor"
      )
    )
  ),
  dashboardSidebar(
    collapsed = TRUE,
    div(textOutput("welcome"), style = "padding: 20px"),
    sidebarMenu(
      menuItem("Dasboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tags$style(HTML(".content { padding: 50px; }")),
    shinyauthr::loginUI(
      id = "login", 
      cookie_expiry = cookie_expiry, 
    ),
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    tags$div(id = "tabs",
      tabItems(
        tabItem(tabName = "dashboard",
                fluidRow(
                  actionButton("add_button", "Add", icon("plus")),
                  actionButton("edit_button", "Edit", icon("edit")),
                  actionButton("delete_button", "Delete", icon("trash-alt")),
                ),
                br(),
                fluidRow(
                         dataTableOutput("responses_table", width = "100%")
                )
        ),
        tabItem(tabName = "widgets"
                
                
        )
      )
    )
    
  )
)

server <- function(input, output, session) {
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  # call login module supplying data frame, user and password cols
  # and reactive trigger
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = users,
    pwd_col = password,
    cookie_logins = TRUE,
    sessionid_col = sessionid,
    cookie_getter = get_sessionids_from_db,
    cookie_setter = add_sessionid_to_db,
    log_out = reactive(logout_init())
  )
  
  observe({
    if (credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
      shinyjs::show("tabs")
      shinyjs::show(selector = ".main-header")
      
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      shinyjs::hide("tabs")
      shinyjs::hide(selector = ".main-header")
    }
  })
  
  user_info <- reactive({
    credentials()$info
  })
  
  output$welcome <- renderText({
    req(credentials()$user_auth)
    
    glue("Bienvenido {user_info()$name}")
  })
  
  # pulls out the user information returned from login module
  user_data <- reactive({
    credentials()$info
  })
  
  output$responses_table <- DT::renderDataTable({
    # use req to only render results when credentials()$user_auth is TRUE
    req(credentials()$user_auth)
    user_data() %>%
      mutate(across(starts_with("login_time"), as.character))
  })
}

shinyApp(ui = ui, server = server)









