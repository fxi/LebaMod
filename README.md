LebaMod
=======

Choose a species, choose predictors and create a new species distribution model ! [LebaMod](http://sdm.unige.ch)

 [LebaMod](http://sdm.unige.ch) is a [R/Shiny](http://www.rstudio.com/products/shiny/) application developed to analyse species presence only dataset with classification models. The name come from Leba, 'Laboratoire d'écologie et biologie acquatique, (Université de Genève)', and mod, statistical modelling. It could be seen as a graphical interface to Caret package ('classification and regression training'), with automated calculation done on remote server and locally managed into a Sqlite database and R single objects for easy filtering and ensemble analysis. The focus is set on the exploration of a potentially large combinaison of models, predictors and parameterers while getting the most accurate spatial and temporal prediction data. Every parameters, method, dataset, predictors used in finals models are collected and can be downloaded for further analysis. It should be quite easy to reproduce every output produced by this application : output objects (class lebaBundle) contain a list filled with species, predictors and parameters data (class lebaData), together with  a standard Caret object (class train/train.formula).

This application needs only a simple sqlite database with two tables: species presence only data and predictors, at the same spatial and temporal extent. 

Currently, the development of this app is suspended, as it reached the special needs of the Leba's researchers. However,  if someone is interested in further development, I could clean and document the underlying functions. 



