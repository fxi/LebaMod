################################################################################
#
# LebaMod
# Laboratoire d'écologie et biologie aquatique, Universté de Genève
# Species presence only modeling and analysis application
#
# moser.frederic@gmail.com
# 2014
#
#
################################################################################



require(shiny)
#require(rCharts)

#conditionalHelper
condHelp<-function(id='showHelp',h){
 conditionalPanel(paste0("input.",id," == true"),
  list(
   em('- ',h,' ')
   )
  )
}


shinyUI( fluidPage(

  titlePanel("LebaMod"),
  h5(sprintf("Laboratoire d'écologie et biologie aquatique : species distribution ensemble modelling for presence only data with automated jobs management on calculation server. (version: 0.1.%s)",system('git rev-list HEAD --count',intern=T))),
  # load javascript ans style file
  htmlOutput('script'),

  tabsetPanel(
   ##############################################################################################
   #
   # INFO
   #
   ##############################################################################################
   tabPanel('Info',
    sidebarPanel(with=5,
     h4('LebaMod'),
     helpText("LebaMod is a tool to analyse species presence only dataset with classification models. The name come from Leba, 'Laboratoire d'écologie et biologie acquatique, (Université de Genève)', and mod, statistical modelling. It could be seen as a graphical interface to Caret package ('classification and regression training'), with automated calculation done on remote server and locally managed into a Sqlite database and R single objects for easy filtering and ensemble analysis. The focus is set on the exploration of a potentially large combinaison of models, predictors and parameterers while getting the most accurate spatial and temporal prediction data. Every parameters, method, dataset, predictors used in finals models are collected and can be downloaded for further analysis. It should be quite easy to reproduce every output produced by this application : output objects (class lebaBundle) contain a list filled with species, predictors and parameters data (class lebaData), together with  a standard Caret object (class train/train.formula). The project itself is completly available in a git repository. Update are made each time new results are available."),
     h4('Input data'),
     helpText('The data used in this application is stored in a single SQLite database with two tables : species presence coordinates and predictors values at the same spatial and time extent. See in attribution part for more information.'),
     h4('Usage'),
     helpText('The main form at left select and filter species, predictors and method data. Every change produce a summary in corresponding tabs : e.g. select a statistical method produce a list of method, with hyperlink to CRAN  (Comprehensive R Archive Network) and set of tags, as defined by the Caret package.'),
     h4('Attributions'),
     h4('Authors'),
     p('moser.frederic@gmail.com'),
     h4('')
     )
    ),





   ##############################################################################################
   #
   # NEW MODEL
   #
   ##############################################################################################
   tabPanel('New models',
    
    sidebarLayout(

     ###################
     #                 #
     # CONFIG SIDE BAR #
     #                 #
     ###################
     sidebarPanel(width=3,
                  hr(),
                  h5(uiOutput('messageLebaMod')),
                  hr(),

      # display help ?
      checkboxInput('showHelp',label='Display help',value=F),
      h4('Email for notification.'),
      #helpText("Could be a disposable email. Enable 'Display help' for more infos "),
      condHelp(h="Your email will be stored only to link the resulting models and send you a notifications when the process is done. No other uses will be attempted. However, this email is stored as is in a database, on our server. We can not guarantee a perfect secure transmission and storage. With this in mind, you could choose to use one from a disposable email service such as mailinator.com. For more informations on this topic, see http://en.wikipedia.org/wiki/Disposable_email_address"),
      textInput('email', 'Email', value = ""),
      hr(),
      h4('Species'),

      condHelp(h="Multiple selection of species to be modelled, based on a list returned by slider 'Range of distinct species sites'. Models based on species distribution with few occurence could fail or be inaccurate. In species tab, a map is updated with every records found in database. Spatial aggregation presented in this map is done by a clustering alogorithm to give a better view of sites spatial distribution."),

      selectInput("selectSpecies", "Select species",
       choices=spL$sp,
       selected='',
       multiple=T
       ),
      checkboxInput("selectAllSpecies","Select all species",FALSE),  
      condHelp(h="Slider 'Range of distinct presence sites' filters the range of distinct presence sites across whole period, for each species. A distinct site of presence is a unique coordinate for a given year. If multiple records are found with same coordinates and same year for a particular species, they are counted as one site."),
      sliderInput("speciesSitesRange","Range of distinct presence sites",
       min=10,
       max=max(spL$nDistinctSite),
       value=c(10,max(spL$nDistinctSite)),
       ticks=TRUE,
       step=1
       ),
      hr(),
      h4('Pseudo-absence generator'),
      condHelp(h='Type of pseudo absence selection : to calibrate presence only model, we need to generate absences with a selection algoritm, e.g. random site sampling. The number of pseudo absence to generate depends of multiple criteria, e.g. type of model (regression/classification) or number of presences available. More informations about selection strategy in barbet2012, lobo2011 and elith2011. In this version of lebaMod, only random spatial sampling with fixed number or multiplicator of pseudo-absence are available. nPa= number of pseudo absence = fixed number, mPa: number of pseudo absence =multiplicator * presence'),
      selectInput('paType','Type of genrator',
       choices=list('Multiplicator (mPa)'='mPa',
        'Fixed number (nPa)'='nPa'
        )),
      conditionalPanel("input.paType == 'mPa'",
       sliderInput(
        inputId='mPa',
        label='Pseudo-absence = number of presence * mPa',
        min=1,
        max=50,
        step=1,
        ticks=T,
        value=1)
       ),
      conditionalPanel("input.paType == 'nPa'",
       sliderInput(
        inputId='nPa',
        label='Pseudo-absence = fixed number for all',
        min=100,
        max=10000,
        step=100,
        ticks=T,
        value=100)
       ),
      hr(),
      h4('Predictors'),
      condHelp(h="Function 'caret::findCorrelation' can help to remove highly correlated predictors before modelisation, based on pair-wise absolute correlation of the training set. With this option set at 1, nothing will be removed, except identical predictors. From the documentation : If two variables have a high correlation, the function looks at the mean absolute correlation of each variable and removes the variable with the largest mean absolute correlation. 
       Predictors with low correlation will be named 'prdLoCorr' and those considered as highly correlated 'prdHiCorr'. Only low correlated ones are used in model. See also in model details/tags to see if it use predictors auto removal internal procedure"),
       sliderInput("corrCutOff","Pair-wise absolute correlation cutoff",
        min=0,
        max=1,
        value=0.5,
        ticks=TRUE,
        step=0.1
        ),
       condHelp(h="Multiple predictors selection.  Predictors descriptions and spatialy summarized time series are available in predictors tab. The minumum number of predictors is 3. "),
       htmlOutput('selectPredictors'),
       htmlOutput('selectAllPredictors'),
      hr(),
       h4('Method'),
       htmlOutput('selectMethods'),
      hr(),
       h4('Group'),
       htmlOutput('selectGroups'),
      hr(),
       h4('Multi-runs'),
      condHelp(h="Compute all pseudo absences selection and models multiple times."),
       htmlOutput('selectRuns'),
       hr(),
       h4('Compute models'),
       actionButton("computeModels", "Compute models")

       ), 
      ###################
      #                 #
      # JOBS METADATA   #
      #                 #
      ###################
      mainPanel(width=9,
       tabsetPanel(

        tabPanel("Species",
         h4('Map of occurences : selected species, all years.'),
         helpText('Display all occurences, possibly with multiple occurences at the same site and same year. Duplicate will be removed during modelling process'),
         htmlOutput('map'),
         htmlOutput('speciesMap'),
         h4('Summary of predictors and spatial data for each presence distribution.'),
         checkboxInput("includeSpatialVars","Add spatial variables in summary",FALSE),
         tableOutput('speciesSummary')

         ),

        tabPanel( "Methods",
         h4('Description of selected methods'),
         tableOutput('describeMethodsTable'),
         h4('List of selected method with'),
         htmlOutput("describeMethods")
         ),
        tabPanel("Predictors",
         h4('Time series of predictors'),
         helpText('Display spatialy aggregated time series for predictors.'),
         plotOutput('describePredictors',width="100%",height="100%"),
         h4('Predictor description list :')
         ),

        tabPanel('Job set',
         h4('Table of jobs to be computed.'),
         tableOutput('jobsTable')
         ),       
        tabPanel('Jobs pending',
         h4('Table of jobs in queue.'),
         tableOutput('jobsPending')
         )

        )
       ))),
    ##############################################################################################
    #
    # RESULTS
    #
    ##############################################################################################
    tabPanel('Results',

     sidebarLayout(

      ###################
      #                 #
      # MODEL FILTER    #
      #                 #
      ###################
      sidebarPanel(width=3,
       hr(),
       uiOutput('messageLebaModResult'),
       hr(),

       h4('Model filter'),

       htmlOutput('filterTss'),
       htmlOutput('filterSpecies'),
       htmlOutput('filterMethod'),
       htmlOutput('filterPredictor'),
       htmlOutput('filterGroup'),

       textInput('filterJob',label='Job id'),
       textInput('filterRun',label='Run id'),
       textInput('filterEmail',label='Email'),



       h4('Predictor filter'),
       htmlOutput('filterYear'),
       htmlOutput('filterDem'),
       htmlOutput('filterLat'),
       htmlOutput('filterLong'),
       selectInput('varPlotBy',
        label='Variable to plot by',
        choices=list(
         Year='YYYY'
         #Elevation='dem',
         #Longitude='x',
         #Latitude='y'
         ))
       #  h5('Manage models database and file'),
       #checkboxInput(inputId='condHideMods',label='Hide models?',value=F),
       #                           conditionalPanel(condition="input.condHideMods == true",
       #                                            helpText("Hide selected models..")
       #                                            actionButton("hideModels", "Delete models")
       #                                            ),


       ),
      ############################
      #                          #
      # RESULTS VISUALISATION    #
      #                          #
      ############################
      mainPanel(tabsetPanel(
        tabPanel('Jobs done',

         h4('Presence probabilities bivariate plot.'),
         actionButton("plotSpVsVar", "Update plot"),
         downloadButton('downloadPlotPresVsYearLat', 'Download plot (lattice/rds)'),
         #downloadButton('downloadPlotPresVsYearPdf', 'Download predictions data (pdf)'),
         plotOutput("spVsVar"),
         hr(),
         h4('Table of jobs available for analyse.'),
         downloadButton('downloadModels', 'Download selected models (zip)'),
         downloadButton('downloadPred', 'Download predictions data (rds)'),

         checkboxInput("showFailed","Show failed",FALSE),
         tableOutput('subModelTable')
         ),
        tabPanel('Animated map by model',
         actionButton("animMap", "Animated map")
         )


        ))
      )
     ),
    tabPanel('R version ',
     h4('R session info on local host.'),
     verbatimTextOutput('RsessionLocal'),
     h4('R session info on remote host.'),
     selectInput('selectRemote','Select remote host',choices=clL,selected='climdal'),
     verbatimTextOutput('RsessionRemote')

     ),
    tabPanel('References',
     includeHTML('www/bib/master.html')

     )
    )#end main tabpanel
   ))#end fluid and shiny
