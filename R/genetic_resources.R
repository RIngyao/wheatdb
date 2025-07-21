#' name_of_module2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

#cultivar_list for selectInput-------------------------------------------

idmap <- read_delim("data-raw/id_map.txt", delim = "\t", col_names = c("variety", "id")) %>% as.data.frame()

genetic_resources_ui <- function(id) {
  ns <- NS(id)
  cultivar_list <- idmap$variety

  tagList(
    div(class = "genetic-resources-ui",

        # Selection Panel
        div(class = "control-panel",
            radioButtons(inputId = ns("user_choice"), inline = TRUE, label = "Display Mode",
                         choices = c("Show all", "Compare"), selected = "Show all"),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'Compare'", ns("user_choice")),
              fluidRow(
                column(6, selectInput(ns("variety1"), "Select Variety 1", choices = cultivar_list)),
                column(6, selectInput(ns("variety2"), "Select Variety 2", choices = cultivar_list))
              )
            )
        ),

        # Display Panel
        # div(class = "image-display", uiOutput(ns("show")))
        uiOutput(ns("show"))
    )
  )
}


#' name_of_module2 Server Functions
#'
#' @noRd
genetic_resources_server <- function(id){
  #addResourcePath("wheatdb", getwd())
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    choice <- reactive(req(input$user_choice))
    select1 <- reactive(input$variety1)
    # option_vec <- unlist(cultivar_list)
    select2 <- reactive(input$variety2)

    cultivar_list <- idmap$variety

    # update the selectinput
    observe({
      req(choice() == "Compare", select1())

      second_var <- reactive(cultivar_list[!cultivar_list %in% select1()])
      # don't change the alread selected value in variet2 if it is not equal to the selection in var1
      old <- ifelse(select1() != input$variety2, input$variety2,second_var()[1])
      # print(second_var())
      updateSelectInput(session,"variety2",
                        choices = second_var(),
                        selected = old #second_var()[1]
      )
    })


   # show the images based on user's choice

    observe({
      req(choice())
      # browser()
      if(choice() == "Show all"){
        output$show <- renderUI({
          tags$iframe(src = "www/genetic.html",
                      width = "100%",
                      height = "900px",
                      style = "border:none") #inst/app/www/seedspike.html
          # test
        })
      } else if(choice() == "Compare"){

        user_input1 <- reactive(input$variety1)
        user_input2 <- reactive(input$variety2)

        select_id1 <- reactive({
          first <- idmap$id[idmap$variety %in% user_input1()]
        })

        select_id2 <- reactive({
          second <- idmap$id[idmap$variety %in% user_input2()]
        })

        output$show <- renderUI({
          div(
            style = " display: flex; padding: 20px; column-gap: 50px;",

            div(
              style = "flex-direction: column;",

              div(
                style = "display: flex; flex-direction: row; padding: 0px; margin: 0px;",
                tags$img(
                  src = paste0("www/images/seeds_", select_id1(),".jpg"), height="200px"
                ),
                tags$img(
                  src = paste0("www/images/spikes_", select_id1(),".JPG"), height = "200px"
                )
              ),

              tags$p(paste0(user_input1()), style = " margin-top: 10px; margin-left: 100px; font-size: 16px; font-weight: bold; color: #333;")
            ),


            div(
              style = "flex-direction: column;",

              div(
                style = "display: flex; flex-direction: row; padding: 0px; margin-left: 30px;",
                tags$img(
                  src = paste0("www/images/seeds_", select_id2(), ".jpg"), height = "200px"
                ),

                tags$img(
                  src = paste0("www/images/spikes_", select_id2(), ".JPG"), height = "200px"
                )

              ),
              tags$p(paste0(user_input2()),  style = " margin-top: 10px; margin-left: 150px; font-size: 16px; font-weight: bold; color: #333;")
            )


          )

        })
      }


    })

  }) # module_server



} # gen_res server


## To be copied in the UI
# mod_name_of_module2_ui("name_of_module2_1")

## To be copied in the server
# mod_name_of_module2_server("name_of_module2_1")
