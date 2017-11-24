# Define UI for dataset viewer application
shinyUI(navbarPage(theme = "bootstrap.css",
                   tags$head(tags$style(HTML("
                                             .shiny-text-output {
                                               background-color:#fff;
                                             }
                                             "))),
                                             tabPanel(uiOutput("ini"),
                                                      useShinyjs(),
                                                      #extendShinyjs(text = jsResetCode),utilizar no futuro para reiniciar o app automaticamente!
                                                      h1(uiOutput("sigla"), 
                                                         span(uiOutput("titulo"), 
                                                              style = "font-weight: 300"), 
                                                         style = "font-family: 'Source Sans Pro';
                                                         color: #fff; text-align: center;
                                                         background-image: url('backg.jpg');
                                                         padding: 20px"),
                                                         br(),
                                                         span(uiOutput("saudacao"),
                                                              style="font-size: 100px; text-align: center; color:blue; font-family: SunSans-Regular, Helvetica, Geneva"),
                                                         withTags({
                                                           div(id="language", class="form-group shiny-input-radiogroup shiny-input-container shiny-input-container-inline",style="text-align: center;",
                                                               div(class="shiny-options-group",
                                                                   div(class="radio-inline",
                                                                       input(type="radio", name="language", value="br"),
                                                                       img(src="brazil.png", height = 100, width = 100)),
                                                                   div(class="radio-inline",
                                                                       input(type="radio", name="language", value="en", checked="checked"),
                                                                       img(src="eua.png", height = 100, width = 100))
                                                                   ))
                                                         })
                                                         ),
                                                      navbarMenu(uiOutput("criarrem"),
                                                                 tabPanel(uiOutput("criar"),
                                                                          column(5,
                                                                                 wellPanel(
                                                                                           uiOutput('ccountry'),
                                                                                           textInput('yourname',
                                                                                                     label = h4(uiOutput("compname")),
                                                                                                     value = 'Achim Zeileis') , 
                                                                                           textInput('text',
                                                                                                     label = h4(uiOutput("escsigla")),
                                                                                                     value = 'ACZ')
                                                                                           )
                                                                                 ),
                                                                          column(5,
                                                                                 wellPanel(
                                                                                           textInput('ydiscp', 
                                                                                                     label = h4(uiOutput("ndiscp")),
                                                                                                     value = 'estatistica_computacional'),
                                                                                           textInput('yassun', 
                                                                                                     label = h4(uiOutput("nassun")), 
                                                                                                     value = 'geracao_numeros_uniformes')
                                                                                           )
                                                                                 ),
                                                                          column(2,
                                                                                 actionButton('gerarb',
                                                                                              uiOutput("gbanco"))
                                                                                 #uiOutput('alert'))
                                                                                 )
                                                                          ),
                                                                 tabPanel(uiOutput("remover"),
                                                                          wellPanel(
                                                                                    uiOutput('removee')
                                                                                    ),
                                                                          verbatimTextOutput('teste'), 
                                                                          actionButton('removerb',
                                                                                       uiOutput("rebanco"))
                                                                          )
                                                                 ),
                                                      tabPanel(uiOutput("criardisc"),
                                                               column(4,
                                                                      wellPanel(
                                                                                uiOutput('discip1'),
                                                                                uiOutput('discip2')
                                                                                ),
                                                                      actionButton("gerard",
                                                                                   uiOutput("gdisc"))
                                                                      ),
                                                               column(5,
                                                                      uiOutput('assunt'),
                                                                      actionButton("gerara",
                                                                                   uiOutput("gassu")) 
                                                                      )),
                                                      #tabPanel(uiOutput("envbanco")),
                                                      navbarMenu(uiOutput("pquest"),
                                                                 tabPanel(uiOutput("tquestaoa"),         
                                                                          tags$meta(content="text/html; charset=utf-8"),
                                                                          uiOutput("exeabertat"),
                                                                          tags$head(tags$script(src="script_savetextarea.js")), 
                                                                          tags$input(type = "button",              
                                                                                     value = "Salvar!",
                                                                                     class = "btn btn-default",
                                                                                     onClick = "saveTextAsFile1()")), 
                                                                 tabPanel(uiOutput("tquestaon"),              
                                                                          tags$meta(content="text/html; charset=utf-8"),
                                                                          uiOutput("exeabertan"),
                                                                          tags$head(tags$script(src="script_savetextarea2.js")), 
                                                                          tags$input(type = "button",              
                                                                                     value = "Salvar!",
                                                                                     class = "btn btn-default",
                                                                                     onClick = "saveTextAsFile2()")),
                                                                 tabPanel(uiOutput("tquestaom"),              
                                                                          tags$meta(content="text/html; charset=utf-8"),
                                                                          uiOutput("exemultiplaes"),
                                                                          tags$head(tags$script(src="script_savetextarea3.js")), 
                                                                          tags$input(type = "button",              
                                                                                     value = "Salvar!",
                                                                                     class = "btn btn-default",
                                                                                     onClick = "saveTextAsFile3()")),   
                                                                 tabPanel(uiOutput("tquestaos"),               
                                                                          tags$meta(content="text/html; charset=utf-8"),
                                                                          uiOutput("exeunicaes"),
                                                                          tags$head(tags$script(src="script_savetextarea4.js")), 
                                                                          tags$input(type = "button",              
                                                                                     value = "Salvar!",
                                                                                     class = "btn btn-default",
                                                                                     onClick = "saveTextAsFile4()")),    
                                                                 tabPanel(uiOutput("associacao"),               
                                                                          tags$meta(content="text/html; charset=utf-8"),
                                                                          uiOutput("exeassociacao"),
                                                                          tags$head(tags$script(src="script_savetextarea5.js")), 
                                                                          tags$input(type = "button",              
                                                                                     value = "Salvar!",
                                                                                     class = "btn btn-default",
                                                                                     onClick = "saveTextAsFile5()")),
                                                                 tabPanel(uiOutput("mista"),              
                                                                          tags$meta(content="text/html; charset=utf-8"), 
                                                                          uiOutput("exemista"),
                                                                          tags$head(tags$script(src="script_savetextarea6.js")), 
                                                                          tags$input(type = "button",              
                                                                                     value = "Salvar!",
                                                                                     class = "btn btn-default",
                                                                                     onClick = "saveTextAsFile6()"))
                                                                 ),
                                                      tabPanel(uiOutput("utbanco"),                     
                                                               column(3,
                                                                      wellPanel(
                                                                                helpText(h4(uiOutput("templ1"))), 
                                                                                textInput('textaval',h5(uiOutput("templ2")),value='1º Avaliação'),
                                                                                textInput('textunid',h5(uiOutput("templ3")),value='Ciências Exatas e Tecnológicas'),
                                                                                textInput('textdisc',h5(uiOutput("templ4")),value='CET173 - Probabilidade e Estatística'),
                                                                                textInput('textcarg',h5(uiOutput("templ5")),value='60 horas'),
                                                                                textInput('textanol',h5(uiOutput("templ6")),value='2017/2'),
                                                                                textInput('textname',h5(uiOutput("templ7")),value='Achim Zeileis')
                                                                                )),
                                                               column(8,
                                                                      wellPanel(
                                                                                numericInput('numquestion',
                                                                                             h4(uiOutput("templ8")),
                                                                                             value=1),
                                                                                helpText(h4(uiOutput("esbanqu"))),

                                                                                source('../../aux_files/widgets/current/widgets.r',local=TRUE)$value

                                                                                ),
                                                                      #verbatimTextOutput('coco'),
                                                                      downloadButton("downloadPDF", uiOutput("botpdf")),
                                                                      downloadButton("downloadXML", uiOutput("botxml")),
                                                                      downloadButton("downloadQuestions", uiOutput("botzip")) 

                                                                      )),
                                                      tabPanel(uiOutput("gdatbanco"),
                                                               wellPanel( 
                                                                         uiOutput("up_file"),
                                                                         actionButton("atubanco",
                                                                                      icon = icon("refresh"),
                                                                                      #width = '400px',
                                                                                      uiOutput("gdatb")))
                                                               ), 
                                                      tabPanel(uiOutput("tutorial"),
                                                               uiOutput("up_guide"))
                                                      )) 
