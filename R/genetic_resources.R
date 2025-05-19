#' name_of_module2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
genetic_resources_ui <- function(id) {
  ns <- NS(id)

  #cultivar_list for selectInput-------------------------------------------
  cultivar_list <- idmap$variety
    # c("Arjun(HD2009)[IC111822]", "PBW-343[IC0240801]", "K-65[IC128211]", "LOK-1[IC144915]",
    #                  "Halna (K-7903)[IC296743]", "HI-617(Sujata)[IC321936]", "Sharbati Sonora[IC384542]", "Agra Local[IC112111]",
    #                  "BANSI-224(GULAB)[IC145237]", "Mundia[IC406697]", "Jhusia[IC564106]", "K-68[IC0128212]", "Vidisha(DL 788-2)[IC0138631]", "VL-829[IC0532689]", "Lal-bahadur[IC0111806]",
    #                  "Motia[IC0111868]", "Narmada-4[IC0111848]","Sonora-64[EC597821]", "HS-240[IC0128195]", "HW-741[IC0128201]", "GW-322[IC0303072]", "AMRITA(HI1500)[IC0296308]",
    #                  "WH -147[IC0393877]", "K-53[IC0443747]", "HD-2888[IC0528118]", "HUW-234(Malvia Wheat-234)[IC0128199]", "NI-5439[IC0073206]",
    #                  "UP-2338[IC0445595]", "[HD2931]", "HD-2932[IC0519900]", "MACS-6222[IC0574481]", "A 090[IC112049]", "NP-4[IC128237]", "[Type-1]",
    #                  "HI-1531(Harshita)[IC527448]", "Kharchia-lal-gehun[IC619437]", "Safed Mundia[IC564129]", "Katha Gehun[IC265322]", "Narmada-l12[IC128236]",
    #                  "C-306[IC128151]", "WR-544(PUSA GOLD)[IC253015]", "Raj-3765[IC0443766]", "Niphad-4[IC0111801]", "Sonalika[EC597826]", "UP-262[IC0128257]",
    #                  "LGM-165[IC128317]", "Pissi-local[IC321856]", "HD-2189[IC0128167]", "HD-2967[IC0574476]", "DWR-225[IC0252526]", "DWR-162[IC0128161]",
    #                  "DWR-16(Keerthi) [IC0075206]", "NP-101[IC0138588]", "Hango 2[IC640652]", "Daulat Khani[IC573144]", "MACS2496[IC128225]", "Mundri[IC107371]",
    #                  "Sathi[IC398298]", "WL711[IC296443]", "8A[IC0111853]", "Narendra Wheat 2036(NW 2036)[IC0443761]", "Dharwad[IC277741]", "NP846[IC128239]",
    #                  "Mondhya[IC138466]", "Type-II[IC0111855]", "GW 2[IC401925]")
 # cultivar_list <- rep(cultivar_list, length.out = 66)

  tagList(



    fluidRow(
      column(8, radioButtons(inputId = ns("user_choice"), inline = TRUE, label = " ", choices = c("Show all", "Compare"))),

      conditionalPanel(condition = sprintf("input['%s'] == 'Compare'", ns("user_choice")),
                       fluidRow(
                         column(12,
                                column(6, selectInput(inputId = ns("variety1"), label = "Select from the following", choices = cultivar_list, selected = NULL)),

                                column(6, selectInput(inputId = ns("variety2"), label = "Select from the following", choices = cultivar_list, selected = NULL)),
                                )
                       ),


      )),

         # actionButton(inputId = ns("click"), label = "Search"),
          htmlOutput(outputId = ns("show")),

    box(
      # two images - total 4 (2 seeds and 2 spikes)
      fluidRow(
        # First options for displaying image choosen by the user
        box(
          fluidRow(
            column(6, imageOutput(ns("seed_img1"))),
            column(6, imageOutput(ns("spike_img1")))
          )
        ),
        # second options
        box(
          fluidRow(
            column(6, imageOutput(ns("seed_img2"))),
            column(6, imageOutput(ns("spike_img2")))
          )
        )
      )
    )
  ,




  )
}

#' name_of_module2 Server Functions
#'
#' @noRd
genetic_resources_server <- function(id){
  #addResourcePath("wheatdb", getwd())
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    #browser()
    choice <- reactive(input$user_choice)
    select1 <- reactive(input$variety1)
   # option_vec <- unlist(cultivar_list)
    #select2 <- reactive(input$variety2)

    idmap <- vroom::vroom("id_map.txt", delim = "\t", col_names = c("variety", "id"))
    print(idmap)

     #image_dir <- "../wheatdb/inst/app/www/images"
     # image_dir_sp <- "../ref_data/images/spikes"
     #  image_dir_sd <- "../ref_data/images/seeds"

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


     # SHOW ALL THE IMAGES
    observe({
        # browser()
        req(choice()=="Show all")
          # browser()
          output$show <- renderUI({
          tags$iframe(src = "www/genetic.html",
                        width = "100%",
                        height = "600px",
                        style = "border:none") #inst/app/www/seedspike.html
            # test
          })

       })

    # update the second select options for user
    observe({
      req(choice() == "Compare", select1())
      # print(select1())
      # req(input$variety1)

      second_var <- reactive(cultivar_list[!cultivar_list %in% select1()])
      # print(second_var())
      updateSelectInput(session,"variety2",
                        choices = second_var(),
                        selected = second_var()[1]
      )


    })


 # browser()
   # name_image <- data.frame(
   #   name <- c("Arjun (HD2009) [IC111822]", "PBW-343 [IC0240801]", "K-65 [IC128211]", "LOK-1 [IC144915]",   #4
   #          "Halna (K-7903) [IC296743]", "HI-617 (Sujata) [IC321936]", "Sharbati Sonora [IC384542]", "Agra Local [IC112111]",  #8
   #          "BANSI-224 (GULAB) [IC145237]", "Mundia [IC406697]", "Jhusia [IC564106]", "K-68 [IC0128212]", "Vidisha (DL 788-2) [IC0138631]", #13
   #          "VL-829 [IC0532689]", "Lal-bahadur [IC0111806]", "Motia [IC0111868]", "Narmada-4 [IC0111848]","Sonora-64 [EC597821]", "HS-240 [IC0128195]", "HW-741 [IC0128201]",
   #          "GW-322 [IC0303072]", "AMRITA (HI1500) [IC0296308]", #20
   #          "WH -147 [IC0393877]", "K-53 [IC0443747]", "HD-2888 [IC0528118]", "HUW-234 (Malvia Wheat-234) [IC0128199]", "NI-5439 [IC0073206]",  #25
   #          "UP-2338 [IC0445595]", "[HD2931]", "HD-2932 [IC0519900]", "MACS-6222 [IC0574481]", "A 090 [IC112049]", "NP-4 [IC128237]", "[Type-1]", #32
   #          "HI-1531 (Harshita) [IC527448]", "Kharchia-lal-gehun [IC619437]", "Safed Mundia [IC564129]", "Katha Gehun [IC265322]", "Narmada-l12 [IC128236]", #37
   #          "C-306 [IC128151]", "WR-544(PUSA GOLD) [IC253015]", "Raj-3765 [IC0443766]", "Niphad-4 [IC0111801]", "Sonalika [EC597826]", "UP-262 [IC0128257]", #43
   #          "LGM-165 [IC128317]", "Pissi-local [IC321856]", "HD-2189 [IC0128167]", "HD-2967 [IC0574476]", "DWR-225 [IC0252526]", "DWR-162 [IC0128161]", #49
   #          "DWR-16 (Keerthi) [IC0075206]", "NP-101 [IC0138588]", "Hango 2 [IC640652]", "Daulat Khani [IC573144]", "MACS2496 [IC128225]", "Mundri [IC107371]", #55
   #          "Sathi [IC398298]", "WL711 [IC296443]", "8A [IC0111853]", "Narendra Wheat 2036(NW 2036) [IC0443761]", "Dharwad [IC277741]", "NP846 [IC128239]",
   #          "Mondhya [IC138466]", "Type-II [IC0111855]", "GW 2 [IC401925]"),
   #
   #
   #   img_id <- c("IC111822", "IC0240801", "IC128211", "IC144915", "IC296743", "IC321936",    "IC384542", "IC112111",   "IC145237",  "IC406697", "IC564106",
   #               "IC0128212", "IC0138631", "IC0532689", "IC0111806", "IC0111868", "IC0111848", "EC597821", "IC0128195", "IC0128201", "IC0303072", "IC0296308",
   #               "IC0393877", "IC0443747", "IC0528118", "IC0128199", "IC0073206", "IC0445595", "HD2931",   "IC0519900", "IC0574481", "IC112049", "IC128237",
   #               "Type-1",    "IC527448", "IC619437", "IC564129", "IC265322",     "IC128236", "IC128151", "IC253015",  "IC0443766", "IC0111801", "EC597826", "IC0128257",
   #               "IC128317", "IC321856", "IC0128167", "IC0574476", "IC0252526", "IC0128161", "IC0075206", "IC0138588", "IC640652",  "IC573144", "IC128225",
   #               "IC107371", "IC398298", "IC296443", "IC0111853", "IC0443761", "IC277741", "IC128239",    "IC138466", "IC0111855",  "IC401925"),
   #
   #
   #    image <- c("2555_IC111822.jpg", "2556_IC0240801.jpg", "2557_IC128211.jpg", "2558_IC144915.jpg", "2559_IC296743.jpg", "seeds_2560_IC321936.jpg", "2561_IC384542.jpg",
   #              "2562_IC112111.jpg", "2563_IC145237.jpg", "2565_IC406697.jpg", "2566_IC564106.jpg", "2567_IC0128212.jpg", "2568_IC0138631.jpg",
   #              "2569_IC0532689.jpg", "2570_IC0111806.jpg", "2571_IC0111868.jpg", "2572_IC0111848.jpg", "2573_EC597821.jpg", "2574_IC0128195.jpg", "2575_IC0128201.jpg",
   #              "2576_IC0303072.jpg", "2577_IC0296308.jpg", "2578_IC0393877.jpg", "2579_IC0443747.jpg", "2580_IC0528118.jpg", "2581_IC0128199.jpg", "2582_IC0073206.jpg",
   #              "2583_IC0445595.jpg", "2584_HD2931.jpg", "2585_IC0519900.jpg", "2586_IC0574481.jpg", "2587_IC112049.jpg", "2588_IC128237.jpg", "2589_IC0111855.jpg",
   #              "2590_Type-1.jpg", "2591_IC527448.jpg", "2592_IC619437.jpg", "2593_IC564129.jpg", "2594_IC265322.jpg", "2595_IC128236.jpg", "2596_IC128151.jpg", "2597_IC253015.jpg",
   #              "2598_IC401925.jpg", "2599_IC0443766.jpg", "2600_IC0111801.jpg", "2601_EC597826.jpg", "2602_IC0128257.jpg", "2603_IC128317.jpg", "2604_IC321856.jpg", "2606_IC0128167.jpg",
   #              "2607_IC0574476.jpg", "2608_IC0252526.jpg", "2609_IC0128161.jpg", "2610_IC0075206.jpg", "2611_IC0138588.jpg", "2612_IC640652.jpg", "2613_IC573144.jpg",
   #              "2614_IC128225.jpg", "2615_IC107371.jpg", "2616_IC398298.jpg", "2617_IC296443.jpg", "2618_IC0111853.jpg", "2628_IC0443761.jpg",
   #              "2629_IC277741.jpg", "2630_IC128239.jpg", "2631_IC138466.jpg")
   # )

 observe({
   # browser()

   # get the image name

   user_input1 <- reactive(input$variety1)
   print(user_input1())
   user_input2 <- reactive(input$variety2)
   print(user_input2())

    select_id1 <- reactive({
     first <- idmap$id[idmap$variety %in% user_input1()]
   })
   print(select_id1())

   select_id2 <- reactive({
     second <- idmap$id[idmap$variety %in% user_input2()]
   })
   print(select_id2())


    output$seed_img1 <- renderImage({
     req(select_id1())
     #seed_path <- file.path("images/", paste0("seeds_", select_id()))
     list(
       src = "inst/app/www/images/seeds_2569_IC0532689.jpg", #paste0("images/seeds_", select_id1(), ".jpg"),
       # contentType = "image/jpg",
       width = 300,
       alt = "seed image")
   }, deleteFile = FALSE)

    output$spike_img1 <- renderImage({
      req(select_id1())
      #seed_path <- file.path("images/", paste0("seeds_", select_id()))
      list(
        src = "inst/app/www/images/seeds_2569_IC0532689.JPG", #paste0("inst/app/www/images/spikes_", select_id1(), "JPG"),
        # contentType = "images/jpg",
        width = 300,
        alt = "spike image")
    }, deleteFile = FALSE)

   # output$spike_img <- renderImage({
   #   req(select_id1(), select_id2())
   #
   #  # spike_path <- file.path("images/", paste0("spikes_", select_id()))
   #   list(
   #     src = paste0("images/spikes_", select_id1()), #, select_id2()),
   #     contentType = "image/JPG",
   #     width = 300,
   #     alt = "Spike image")
   # })

})


    }) # module_server



} # gen_res server


## To be copied in the UI
# mod_name_of_module2_ui("name_of_module2_1")

## To be copied in the server
# mod_name_of_module2_server("name_of_module2_1")
