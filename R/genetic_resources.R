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

idmap <- read_delim("id_map.txt", delim = "\t", col_names = c("variety", "id")) %>% as.data.frame()

genetic_resources_ui <- function(id) {
  ns <- NS(id)
  cultivar_list <- idmap$variety
  # c("Arjun(HD2009)[IC111822]", "PBW-343[IC0240801]", "K-65[IC128211]", "LOK-1[IC144915]",
  #                    "Halna(K-7903)[IC296743]", "HI-617(Sujata)[IC321936]", "Sharbati Sonora[IC384542]", "Agra Local[IC112111]",
  #                    "BANSI-224(GULAB)[IC145237]", "Mundia[IC406697]", "Jhusia[IC564106]", "K-68[IC0128212]", "Vidisha(DL 788-2)[IC0138631]", "VL-829[IC0532689]", "Lal-bahadur[IC0111806]",
  #                    "Motia[IC0111868]", "Narmada-4[IC0111848]","Sonora-64[EC597821]", "HS-240[IC0128195]", "HW-741[IC0128201]", "GW-322[IC0303072]", "AMRITA(HI1500)[IC0296308]",
  #                    "WH -147[IC0393877]", "K-53[IC0443747]", "HD-2888[IC0528118]", "HUW-234(Malvia Wheat-234)[IC0128199]", "NI-5439[IC0073206]",
  #                    "UP-2338[IC0445595]", "[HD2931]", "HD-2932[IC0519900]", "MACS-6222[IC0574481]", "A 090[IC112049]", "NP-4[IC128237]", "[Type-1]",
  #                    "HI-1531(Harshita)[IC527448]", "Kharchia-lal-gehun[IC619437]", "Safed Mundia[IC564129]", "Katha Gehun[IC265322]", "Narmada-l12[IC128236]",
  #                    "C-306[IC128151]", "WR-544(PUSA GOLD)[IC253015]", "Raj-3765[IC0443766]", "Niphad-4[IC0111801]", "Sonalika[EC597826]", "UP-262[IC0128257]",
  #                    "LGM-165[IC128317]", "Pissi-local[IC321856]", "HD-2189[IC0128167]", "HD-2967[IC0574476]", "DWR-225[IC0252526]", "DWR-162[IC0128161]",
  #                    "DWR-16(Keerthi) [IC0075206]", "NP-101[IC0138588]", "Hango 2[IC640652]", "Daulat Khani[IC573144]", "MACS2496[IC128225]", "Mundri[IC107371]",
  #                    "Sathi[IC398298]", "WL711[IC296443]", "8A[IC0111853]", "Narendra Wheat 2036(NW 2036)[IC0443761]", "Dharwad[IC277741]", "NP846[IC128239]",
  #                    "Mondhya[IC138466]", "Type-II[IC0111855]", "GW 2[IC401925]")


  tagList(
    fluidRow(

      column(8, radioButtons(inputId = ns("user_choice"), inline = TRUE, label = "Choose", choices = c("Show all", "Compare"))),

      conditionalPanel(condition = sprintf("input['%s'] == 'Compare'", ns("user_choice")),

                       fluidRow(
                         column(12,
                                column(6, selectInput(inputId = ns("variety1"), label = "Select from the following", choices = cultivar_list, selected = NULL)),

                                column(6, selectInput(inputId = ns("variety2"), label = "Select from the following", choices = cultivar_list, selected = NULL))
                         ) # column end
                       )# end of inner fluidrow
      )


    ),


    # use uioutput instead of htmloutput
    uiOutput(ns("show"))

  ) # end of tagList
}

#' name_of_module2 Server Functions
#'
#' @noRd
genetic_resources_server <- function(id){
  #addResourcePath("wheatdb", getwd())
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    choice <- reactive(input$user_choice)
    select1 <- reactive(input$variety1)
    # option_vec <- unlist(cultivar_list)
    select2 <- reactive(input$variety2)

    cultivar_list <- idmap$variety

    # update the selectinput
    observe({
      req(choice() == "Compare", select1())

      second_var <- reactive(cultivar_list[!cultivar_list %in% select1()])
      # print(second_var())
      updateSelectInput(session,"variety2",
                        choices = second_var(),
                        selected = second_var()[1]
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
                      height = "600px",
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
                  src = paste0("www/images/spikes_", select_id2(),".JPG"), height = "200px"
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
