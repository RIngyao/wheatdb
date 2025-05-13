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
   #options for selectInput-------------------------------------------
     options <- c("Arjun (HD2009) [IC111822]", "PBW-343 [IC0240801]", "K-65 [IC128211]", "LOK-1 [IC144915]",
                  "Halna (K-7903) [IC296743]", "HI-617 (Sujata) [IC321936]", "Sharbati Sonora [IC384542]", "Agra Local [IC112111]",
                  "BANSI-224 (GULAB) [IC145237]", "Mundia [IC406697]", "Jhusia [IC564106]", "K-68 [IC0128212]", "Vidisha (DL 788-2) [IC0138631]", "VL-829 [IC0532689]", "Lal-bahadur [IC0111806]",
                  "Motia [IC0111868]", "Narmada-4 [IC0111848]","Sonora-64 [EC597821]", "HS-240 [IC0128195]", "HW-741 [IC0128201]", "GW-322 [IC0303072]", "AMRITA (HI1500) [IC0296308]",
                  "WH -147 [IC0393877]", "K-53 [IC0443747]", "HD-2888 [IC0528118]", "HUW-234 (Malvia Wheat-234) [IC0128199]", "NI-5439 [IC0073206]",
                  "UP-2338 [IC0445595]", "[HD2931]", "HD-2932 [IC0519900]", "MACS-6222 [IC0574481]", "A 090 [IC112049]", "NP-4 [IC128237]", "[Type-1]",
                  "HI-1531 (Harshita) [IC527448]", "Kharchia-lal-gehun [IC619437]", "Safed Mundia [IC564129]", "Katha Gehun [IC265322]", "Narmada-l12 [IC128236]",
                  "C-306 [IC128151]", "WR-544(PUSA GOLD) [IC253015]", "Raj-3765 [IC0443766]", "Niphad-4 [IC0111801]", "Sonalika [EC597826]", "UP-262 [IC0128257]",
                  "LGM-165 [IC128317]", "Pissi-local [IC321856]", "HD-2189 [IC0128167]", "HD-2967 [IC0574476]", "DWR-225 [IC0252526]", "DWR-162 [IC0128161]",
                  "DWR-16 (Keerthi) [IC0075206]", "NP-101 [IC0138588]", "Hango 2 [IC640652]", "Daulat Khani [IC573144]", "MACS2496 [IC128225]", "Mundri [IC107371]",
                  "Sathi [IC398298]", "WL711 [IC296443]", "8A [IC0111853]", "Narendra Wheat 2036(NW 2036) [IC0443761]", "Dharwad [IC277741]", "NP846 [IC128239]",
                  "Mondhya [IC138466]", "Type-II [IC0111855]", "GW 2 [IC401925]")

  tagList(

    fluidRow(
      column(8, radioButtons(inputId = ns("user_choice"), inline = TRUE, label = " ", choices = c("Show all", "Select"))),

      conditionalPanel(condition = sprintf("input['%s'] == 'Select'", ns("user_choice")),
                       fluidRow(
                         column(12,
                                column(6, selectInput(inputId = ns("variety"), label = "select from the following", choices = options, selected = NULL)),

                                column(6, selectInput(inputId = ns("variety1"), label = "select from the following", choices = options, selected = NULL)),
                                )
                       ),


      )),

          actionButton(inputId = ns("click"), label = "Search"),
          htmlOutput(outputId = ns("show")),

  fluidRow(
    column(6, imageOutput("seed_img")),
    column(6, imageOutput("spike_img"))
  ),




  )
}

#' name_of_module2 Server Functions
#'
#' @noRd
genetic_resources_server <- function(id){
  #addResourcePath("wheatdb", getwd())
  moduleServer(id, function(input, output, session){
    ns <- session$ns

 #  browser()
    choice <- reactive(input$user_choice)
    select <- reactive(input$variety)
    option_rec <- reactive(options())



     idmap <- vroom::vroom("../wheatdb/id_map.txt", col_names = FALSE, delim = "\t")
     colnames(idmap) <- c("variety", "id")
     image_dir_sd <- "../ref_data/images/seeds"
     image_dir_sp <- "../ref_data/images/spikes"




    observeEvent(input$click,  {
    # browser()
    req(choice())

      if(choice() == "Show all") {

        output$show <- renderUI({

          shiny::addResourcePath(system.file("app/www/test.html", package = "wheatdb"))

          # tags$iframe(src="../wheatdb/inst/app/www/seedspike.html", height = 600, width = 600)
          # test <- tags$iframe("seedspike.html", height = 1000, width = 1000)
          includeHTML("test.html")
          # includeHTML(system.file("app/www/test.html", package = "wheatdb")) #../wheatdb/inst/app/
          # test <- tags$iframe(id="gr", src = "www.google.com") #inst/app/www/seedspike.html
          # test
        })
      }
#


      else if (choice() == "Select") {

        update_choice <- reactive({
          choice <- option_rec[!option_rec() %in% select()]
          return(choice)
        })
       observe({
         req(select())

         updateSelectInput(session,
           inputId = "variety1",
           choices = update_choice(),
           label = NULL,
           selected = NULL
         )
       })


     # browser()

    # observe({
    #
    #   select_id <- reactive({
    #     req(input$variety)
    #     idmap$id[idmap$variety == input$variety]
    #   })
    #
    #
        name_image <- data.frame(
          name <- c("Arjun (HD2009) [IC111822]", "PBW-343 [IC0240801]", "K-65 [IC128211]", "LOK-1 [IC144915]",   #4
                 "Halna (K-7903) [IC296743]", "HI-617 (Sujata) [IC321936]", "Sharbati Sonora [IC384542]", "Agra Local [IC112111]",  #8
                 "BANSI-224 (GULAB) [IC145237]", "Mundia [IC406697]", "Jhusia [IC564106]", "K-68 [IC0128212]", "Vidisha (DL 788-2) [IC0138631]", #13
                 "VL-829 [IC0532689]", "Lal-bahadur [IC0111806]", "Motia [IC0111868]", "Narmada-4 [IC0111848]","Sonora-64 [EC597821]", "HS-240 [IC0128195]", "HW-741 [IC0128201]",
                 "GW-322 [IC0303072]", "AMRITA (HI1500) [IC0296308]", #20
                 "WH -147 [IC0393877]", "K-53 [IC0443747]", "HD-2888 [IC0528118]", "HUW-234 (Malvia Wheat-234) [IC0128199]", "NI-5439 [IC0073206]",  #25
                 "UP-2338 [IC0445595]", "[HD2931]", "HD-2932 [IC0519900]", "MACS-6222 [IC0574481]", "A 090 [IC112049]", "NP-4 [IC128237]", "[Type-1]", #32
                 "HI-1531 (Harshita) [IC527448]", "Kharchia-lal-gehun [IC619437]", "Safed Mundia [IC564129]", "Katha Gehun [IC265322]", "Narmada-l12 [IC128236]", #37
                 "C-306 [IC128151]", "WR-544(PUSA GOLD) [IC253015]", "Raj-3765 [IC0443766]", "Niphad-4 [IC0111801]", "Sonalika [EC597826]", "UP-262 [IC0128257]", #43
                 "LGM-165 [IC128317]", "Pissi-local [IC321856]", "HD-2189 [IC0128167]", "HD-2967 [IC0574476]", "DWR-225 [IC0252526]", "DWR-162 [IC0128161]", #49
                 "DWR-16 (Keerthi) [IC0075206]", "NP-101 [IC0138588]", "Hango 2 [IC640652]", "Daulat Khani [IC573144]", "MACS2496 [IC128225]", "Mundri [IC107371]", #55
                 "Sathi [IC398298]", "WL711 [IC296443]", "8A [IC0111853]", "Narendra Wheat 2036(NW 2036) [IC0443761]", "Dharwad [IC277741]", "NP846 [IC128239]",
                 "Mondhya [IC138466]", "Type-II [IC0111855]", "GW 2 [IC401925]"),

          image <- c("2555_IC111822.jpg", "2556_IC0240801.jpg", "2557_IC128211.jpg", "2558_IC144915.jpg", "2559_IC296743.jpg", "2560_IC321936.jpg", "2561_IC384542.jpg",
                     "2562_IC112111.jpg", "2563_IC145237.jpg", "2565_IC406697.jpg", "2566_IC564106.jpg", "2567_IC0128212.jpg", "2568_IC0138631.jpg",
                     "2569_IC0532689.jpg", "2570_IC0111806.jpg", "2571_IC0111868.jpg", "2572_IC0111848.jpg", "2573_EC597821.jpg", "2574_IC0128195.jpg", "2575_IC0128201.jpg",
                     "2576_IC0303072.jpg", "2577_IC0296308.jpg", "2578_IC0393877.jpg", "2579_IC0443747.jpg", "2580_IC0528118.jpg", "2581_IC0128199.jpg", "2582_IC0073206.jpg",
                     "2583_IC0445595.jpg", "2584_HD2931.jpg", "2585_IC0519900.jpg", "2586_IC0574481.jpg", "2587_IC112049.jpg", "2588_IC128237.jpg", "2589_IC0111855.jpg",
                     "2590_Type-1.jpg", "2591_IC527448.jpg", "2592_IC619437.jpg", "2593_IC564129.jpg", "2594_IC265322.jpg", "2595_IC128236.jpg", "2596_IC128151.jpg", "2597_IC253015.jpg",
                     "2598_IC401925.jpg", "2599_IC0443766.jpg", "2600_IC0111801.jpg", "2601_EC597826.jpg", "2602_IC0128257.jpg", "2603_IC128317.jpg", "2604_IC321856.jpg", "2606_IC0128167.jpg",
                     "2607_IC0574476.jpg", "2608_IC0252526.jpg", "2609_IC0128161.jpg", "2610_IC0075206.jpg", "2611_IC0138588.jpg", "2612_IC640652.jpg", "2613_IC573144.jpg",
                     "2614_IC128225.jpg", "2615_IC107371.jpg", "2616_IC398298.jpg", "2617_IC296443.jpg", "2618_IC0111853.jpg", "2628_IC0443761.jpg",
                     "2629_IC277741.jpg", "2630_IC128239.jpg", "2631_IC138466.jpg")
        )
    #   output$seed_img <- renderImage({
    #     id <- select_id


        # get the image name
        img <- name_image[name_image$name == input$variety, "image"]
         output$seed_img <- renderImage({
           img
         })

    #     seed_path <- file.path(image_dir_sd, paste(image_dir_sd, img))
    #
    #    print(list.files(pattern = '*\\.jpg'))
    #
    #
    #
    #   })
    #
    #   output$spike_img <- renderImage({
    #     id <- select_id()
    #     spike_path <- file.path(image_dir_sp, paste(image_dir_sp, "_.JPG"))
    #
    #    print(list.files(pattern = '*\\.JPG'))
    #   })
    #
    #
    # })




          HTML('

          <select id = "variety-pair" onchange = "updateImg">
          <option value="IC111822">Arjun(HD2009)[IC111822]</option>
          <option value="IC0240801">PBW-343[IC0240801]</option>
          <option value="IC128211">K-65[[IC128211]</option>
          <option value="IC144915">LOK-1[IC144915]</option>
          <option value="IC296743">Halna(K-7903)</option>
          <option value="IC321936">HI-617(Sujata)[IC321936]</option>
          <option value="IC384542">Sharbati Sonora[IC384542]</option>
          <option value="IC112111">Agra Local[IC112111]</option>
          <option value="IC145237">BANSI-224(GULAB)[IC145237]</option>
          <option value="IC406697">Mundia[IC406697]</option>
        </select>


           <div class = "img-group">
            <h3>Seed</h3>
           <img id = "seed-img" src = "" alt = "Seed image of wheat">
            <h3>Spike</h3>
           <img id = "spike-img src = "" alt= "spike image of wheat">
           </div>

          <script>
           function updateImg() {
           const select = document.getElementById("variety-select")  ;
            const id =select.value;

           if(id) {
           document.getElementById("seed-img").src = "../ref_data/${id}_seed-img";
           document.getElementById("spike-img").src = "ref_data/${id}_spike-img" ;
           }


           }

          </script>


          ')

      }
    })



    # observeEvent(input$click, {
    #   req(choice())
    #
    #   if(choice() == "Show all") {
    #
    #    filename <- system.file("jpg", "../ref_data/images")
    #      file <- "../ref_data/images"
    #
    #    output
    #
    #
    #   }
    #     else if(choice() == "Select") {
    #
    #     }
    #
    #
    # })
    #


  })
}

## To be copied in the UI
# mod_name_of_module2_ui("name_of_module2_1")

## To be copied in the server
# mod_name_of_module2_server("name_of_module2_1")
