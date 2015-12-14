###Project Title: Global Terrorism Attack Attribution
*Project goal*: This project is undertaken to contribute to the problem of identifying the perpetrators of "unknown" terrorist attacks.<br/>
*Project description*:The project consists of four stage: 
1. Data exploration & preparation
2. Data treatment
3. Data modelling
4. Model evaluation.<br/><br/>
**Data exploration & prepartion** <br/>
During the initial stage, I performed the following operations:<br/>
1.1 Original data set (http://www.start.umd.edu/gtd/) loading. As a result of this step, 141,966 observations of 134 variables were loaded.
1.2 Variable identification. The task was "to build a model that can predict what group may have been responsible for an incident" using "attack type, weapons used, description of the attack, etc.", therefore, target variable is "gname" or Perpetrator Group Name (Global Terrorism Database, codebook: inclusion criteria and variables) whereas 134 variables are potential input variables.

    terrorist_prep <- read.csv("C:/Users/eponkratov003/Desktop/challenge/final/dataset.csv", header=TRUE, sep=",")
    str(terrorist_prep)
    unique(terrorist_prep$gname)
    
1.3 Variables exploration. There are 3189 unique observations in the column "gname" with 71% of terrorist groups responsible for 3 and below terrorist acts. 

    terrorist_prep_plot <- read.csv("C:/Users/eponkratov003/Desktop/challenge/final/dataset.for.plot.csv", header=TRUE, sep=",")
    tb <- table(terrorist_prep_plot$gname)
    tb2 <- tb[tb>3]
    barplot(tb2[order(tb2, decreasing=T)], xaxt='n', ann=FALSE)
    axis(2, at=seq(0, 0, 1))
    library(plyr)
    a <- count(terrorist_prep_plot, 'gname')
    a[order(a[["freq"]], decreasing = FALSE), ]
    
To provide adequate numbers for statistical analysis, having the names of the terrorist groups, I would collected additional attribute i.e. nationality or headquarters of the groups (GTD staff used the following division in the data collection: North America, Central America & Caribbean, South America, East Asia, Southeast Asia, South Asia, Central Asia, Western Europe, Eastern Europe, Middle East & North Africa, Sub‐Saharan Africa, Australasia & Oceania, and others) and then assign groups to corresponding categories. The main drawback of the first approach is that I would be able to predict regions of terrorist groups origin, not group names.<br/><br/>
**Data exploration & prepartion** <br/>
For the analysis, I used the data from the Global Terrorism Database. The Database was collected by the Pinkerton Global Intelligence Service (PGIS), a private security agency in the period between 1970 and 1997. Cases that occurred between 1998 and March 2008 were collected by the Center for Terrorism and Intelligence Studies (CETIS); events that occured between April 2008 and October 2011 were coded by the Institute for the Study of Violent Groups (ISVG) at the University of New Haven. Finally, starting from November 2011 until this date, GTD data collection is done by START staff at the University of Maryland.<br/> 
START changed the definition of terrorism that they used to identify terrorist events for 1998 and thereafter. The GTD defines a terrorist attack as *the threatened or actual use of illegal force and violence by a non‐state actor to attain a political, economic, religious, or social goal through fear, coercion, or intimidation. In practice this means in order to consider an incident for inclusion in the GTD, all three of the following attributes must be present: the incident must be intentional; the incident must entail some level of violence or immediate threat of violence; the perpetrators of the incidents must be sub‐national actors.* Therefore, after 1997, I excluded attacks that do not satisfy the above criteria. As there will be just 13 output  categories, I did not partion input categories. However, I excluded "Unarmed Assault" and "Unknown" attacks because they cannot be attributed to a particular type of attack. Moreover, "Terrorists/Non-State Militia", "Unknown" and "Others" objects were also excluded as targets because those acts are committed on nonterrorist targets. The "dataset_modified_v2" dataset included 70% (99,243) of the original dataset (141,966). The list of 98,666 was split into train (55,198 events) and test(43,468 events) datasets.</br>

    terrorist_prep_subset <- subset(terrorist_prep, crit1 == 1 & crit2 == 1 & crit3 == 1 & doubtterr == 0 & targtype1_txt !="Terrorists/Non-State Militia" & targtype1_txt !="Unknown" & targtype1_txt !="Other" & attacktype1_txt !="Unarmed Assault" & attacktype1_txt !="Unknown")
    str(terrorist_prep_subset)
    terrorist_prep_tidy <- terrorist_prep_subset[,c("iyear","imonth","country_txt", "region_txt","success", "suicide","attacktype1_txt","targtype1_txt","natlty1_txt","gname","claimed","claimmode_txt","weaptype1_txt","INT_LOG", "INT_IDEO")]
    str(terrorist_prep_tidy)
    write.csv(terrorist_prep_tidy, "C:/Users/eponkratov003/Desktop/challenge/final/dataset_modified_v2.csv")
    
Even though the dataset was decreased by 3/10, but it was considerably big and had around 49 features. To cut down on runtime, the random.forest.importance function was implemented. 

    library(ElemStatLearn)
    library(FSelector)
    train_decision_tree <- read.csv("C:/Users/eponkratov003/Desktop/challenge/final/with features selected/train_decision_tree.csv", header=TRUE, sep=",")
    att.scores <- random.forest.importance(group_headquarter.add. ~ region_txt +attacktype1_txt + targtype1_txt+natlty1_region.add. +weaptype1_txt + iyear + INT_LOG + INT_IDEO, train_decision_tree)
    att.scores
    cutoff.biggest.diff(att.scores)
    cutoff.k(att.scores, k = 5) 
    
The cutoff.biggest diff fuction identified the features that had a significantly higher importance value than other features.<br/>
*region_txt attr_importance*: 187.14682<br/>
*attacktype1_txt attr_importanc*e: 71.72610<br/>
*targtype1_txt attr_importance*: 113.17202<br/>
*natlty1_region.add. attr_importance*: 106.44835</br>
*weaptype1_txt attr_importance*: 78.51758 <br/>
*iyear attr_importance*: 104.13608 </br>
*INT_LOG attr_importance*: 67.82896</br>
*INT_IDEO attr_importance*: 56.55586</br>
[1] "region_txt"          "targtype1_txt"       "natlty1_region.add." "iyear"               "weaptype1_txt"     
Based on the above ranking, four attributes: region_txt, targtype1_txt, natlty1_region.add, and iyear were used. <br/><br/>
**Model evaluation**<br/>
To assess the model's performance, accuracy, precision, recall and f-measure were estimated. The model achieved 96% in precision and 99% in recall.

    library(rpart)
    terrorist_tree <- rpart(group_headquarter.add. ~ region_txt + targtype1_txt + natlty1_region.add. + iyear, data = train_decision_tree, method = "class")
    plot(terrorist_tree)
    text(terrorist_tree)
    terrorist_tree_predictions <- predict(terrorist_tree, train_decision_tree, type = "class")
    terrorist_tree_confusion <- table(terrorist_tree_predictions, train_decision_tree$group_headquarter.add.)
    print(terrorist_tree_confusion)
    terrorist_tree_accuracy <- sum(diag(terrorist_tree_confusion)) / sum(terrorist_tree_confusion)
    print(terrorist_tree_accuracy)
    terrorist_tree_precision <- terrorist_tree_confusion[2,2] / sum(terrorist_tree_confusion[2,])
    print(terrorist_tree_precision)
    terrorist_tree_recall <- terrorist_tree_confusion[2,2] / sum(terrorist_tree_confusion[,2])
    print(terrorist_tree_recall)
    terrorist_tree_inal <- 2 * terrorist_tree_precision * terrorist_tree_recall/ (terrorist_tree_precision + terrorist_tree_recall)
    print(terrorist_tree_inal)

print(terrorist_tree_accuracy)<br/>
[1] 0.8457734 <br/>
print(terrorist_tree_precision)<br/>
[1] 0.9592157<br/>
print(terrorist_tree_recall)<br/>
[1] 0.995118<br/>
print(terrorist_tree_inal)<br/>
[1] 0.9768371

    terrorist_tree_test <- read.csv("C:/Users/eponkratov003/Desktop/challenge/final/with features selected/test_decision_tree.csv")
    my_prediction <- predict(terrorist_tree, terrorist_tree_test, type = "class")
    my_solution <- data.frame(region_txt = terrorist_tree_test$region_txt, targtype1_txt = terrorist_tree_test$targtype1_txt, natlty1_region.add. = terrorist_tree_test$natlty1_region.add., iyear = terrorist_tree_test$iyear, group_headquarter.add. = my_prediction)
    nrow(my_solution)
    write.csv(my_solution, file = "C:/Users/eponkratov003/Desktop/challenge/final/with features selected/terrorist_tree_test_country_of_origin.csv", row.names = FALSE)
