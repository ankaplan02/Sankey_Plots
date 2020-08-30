
suppressPackageStartupMessages(require(googleVis))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(RColorBrewer))
suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(library(shinyWidgets))
suppressPackageStartupMessages(library(lattice))

#functions for string maneuvering in Sankey 
source("functions_Sankey.R")

# Data #

load(file.path("data", "Sim_Data_Sankey_Long.RData")) # data_long
ageRange <- range(data_long$Age)
Networks <- unique(data_long$Network)
Networks <- Networks[order(Networks)]

Ethnicity <- unique(data_long$ethnic)

# Define UI for application that draws a histogram
ui <- fluidPage(
      fluidRow(
    # Application title
    titlePanel("Example Sankey Plot Using Fake Data"),
    # Copy the line below to make a slider bar 
    wellPanel(column(11,
    column(3, sliderInput("age", label = "Desired Age Range:", min = ageRange[1], 
                max = ageRange[2], value = c(ageRange[1],ageRange[2]))),
    
    column(3, pickerInput("network","Specific Networks:", Networks, selected = Networks, options = list(`actions-box` = TRUE,`selected-text-format`= "count > 5",
                                                                              `count-selected-text` = "All Networks"), multiple = TRUE)),
    
    column(3, pickerInput("eth","Specified Ethnicities:", Ethnicity, selected = Ethnicity, options = list(`actions-box` = TRUE,`selected-text-format`= "count > 5",
                                                                              `count-selected-text` = "All Ethnicities"), multiple = TRUE)),
    offset = 2)),
    column(10, uiOutput(HTML("timelabs")), offset = 3),
    column(12, column(3, plotOutput("legend")), column(9, "", column(9,"", htmlOutput("distPlot")))),
    #column(12, column(2, h3("Total Proportions at Each Time Point:")), column(10, "", fixedRow(10, plotOutput("stacked")))),
    column(12, column(2, h3("Total Proportions at Each Time Point:")), column(10, "", fixedRow(column(10, htmlOutput("barchart"))))),
    column(12, column(2, h3("Age Spread at each Time Point:")), column(10, "", fixedRow(column(10, "", plotOutput("ageD"))))),
    column(12, column(2, h3("Ethnicity Frequencies at each Time Point:")), column(10, "", fixedRow(column(10, "", plotOutput("ethChart"))))),
    column(12, column(2, h3("Network Frequencies at each Time Point:")), column(10, "", fixedRow(column(10, "", plotOutput("NChart")))))
    
        ) 
    )
      
# Define server logic required to draw a histogram
server <- function(input, output) {

    #manipulate data (summarize)
    getsub <- reactive({
      
      shiny::validate(
        need(input$eth, "Please select at least one ethnicity"),
        need(input$network, "Please select at least one network")#,
      ) 
      
      data_long2 <- data_long %>%
        filter(Age <= input$age[2] & Age >= input$age[1]) %>%
        filter(Network %in% input$network) %>%
        filter(ethnic %in% input$eth) #%>%
      
      # creating Sankey Data #
      # time point should be the same for each data set if longitudinal
      times <- as.character(unique(data_long2$Time))
      pairtime <- t(sapply(1:4, function(i) c(times[i], times[i+1])))
      # how can I tell the column for the groupings of patients? 
      # not every data set will have "drug"... 
      # if all of them have eth, netowrk, age, and time, then just get
      # that name then that's left out
      #names(data_long2)
      levs <- as.character(unique(data_long2$Drug))
      levs <- levs[order(levs)]
      data_long2$Drug <- factor(data_long2$Drug, levels = levs)
      pairsnum <- lapply(1:4, function(x) table(data_long2$Drug[data_long2$Time == pairtime[x,1]],
                                                data_long2$Drug[data_long2$Time == pairtime[x,2]]))
      uniqueDrugs <- unique(data_long2$Drug)
      uniqueDrugs <- uniqueDrugs[order(uniqueDrugs)]
      numD <- length(uniqueDrugs)
      numTime <- length(times) - 1
      frame1 <- as.character(rep(uniqueDrugs, numD))
      frame2 <- c(sapply(1:numD, function(x) rep(uniqueDrugs[x], numD)))
      frame <- cbind(frame1, frame2)
      frame <- do.call(rbind, replicate(numTime, frame, simplify=FALSE))
      
      unlistwithin <- lapply(1:numTime, function(x) as.numeric(pairsnum[[x]]))
      unlistwithin <- as.data.frame(cbind(unlist(unlistwithin), frame))
      names(unlistwithin) <- c("Patients", "from", "to")
      
      
      unlistwithin$from <- paste(sort(rep(times[1:numTime], numD^2)), unlistwithin$from,
                                 sep = " ")
      unlistwithin$to <- paste(sort(rep(times[2:(numTime+1)], numD^2)), unlistwithin$to,
                               sep = " ")
      # slowly getting the data into sankey format
      # these last couple of lines creates the sourceNodes and targetNodes
      # labels, that will be used later
      # also, the time comes first, then the Drug initial for these node names
      # this will have to change (the substring lines) for however long the drug names are
      
      unlistwithin$Patients <- as.numeric(as.character(unlistwithin$Patients))
      #unlistwithin$from <- as.factor(unlistwithin$from);
      #unlistwithin$to <- as.factor(unlistwithin$to);
      unlistwithin <- unlistwithin[,c(3,2,1)]
      
      # thsi line will have to have supplementary data
      # such that we can use the extra data, which will have labels
      # for times and drug names, to obtain the proper substrings
      # in teh sankey formed data, as for now... it's fine haha
      unlistwithin$bigtime <- substring(unlistwithin$from, 1, 6); 
      unlistwithin$drugname <- substring(unlistwithin$from, 8)
      # gets the drug name and time name
      timeto <- substring(unlistwithin$to, 1, 6)
      drugto <- substring(unlistwithin$to, 8)
      # drug name and time name for the target
      unlistwithin$from <- paste(unlistwithin$drugname, unlistwithin$bigtime, sep = "-")
      unlistwithin$to <- paste(drugto, timeto, sep = "-")
      unlistwithin <- unlistwithin %>% arrange(bigtime,drugname)
      #unlistwithin2 <- unlistwithin[,c(4, 5)]
      #unlistwithin2$from <- factor(unlistwithin2$from, levels = (unique(unlistwithin2$from)))
      #unlistwithin2$to <- factor(unlistwithin2$to, levels = (unique(unlistwithin2$to)))
      unlistwithin2 <- unlistwithin[,c(2,1,3,4,5)]
    
      #forSankey <- forSankey %>% jsonlite::toJSON()
      
      names(unlistwithin2)[c(1,2,3)] <- c("sourceNode", "targetNode", "Patients")
      
      # creates summary numbers for trajectories 
      # but not total patients in a node at a certaint ime point
      
      source_ns <- unlistwithin2 %>%
        group_by(sourceNode) %>%
        summarise(sourceN = sum(Patients)) %>%
        ungroup()
      
      target_ns <- unlistwithin2 %>%
        group_by(targetNode) %>%
        summarise(targetN = sum(Patients)) %>%
        ungroup()
      
      unlistwithin2$Patients <- as.numeric(unlistwithin2$Patients)
      
      # googleVis preparation #
      
      dat2_reordered <- unlistwithin2
      
      uniqueTarget <- unique(dat2_reordered$targetNode)
      uniqueSource <- unique(dat2_reordered$sourceNode)
      
      # now creating data for total counts per node for ggplot2 to 
      # have a stacked bar chart with matching colors as the sankey chart
      dat2_reordered <- dat2_reordered %>%
        inner_join(source_ns, by = 'sourceNode') %>%
        inner_join(target_ns, by = 'targetNode') %>%
        mutate(
          to_from = paste0('<b>', sourceNode, " &rArr; ", targetNode, ':</b>'))
      
      dat3 <- dat2_reordered %>% select(bigtime, targetNode, targetN) %>% 
        mutate(targetTime = substring(targetNode,3,8)) %>% unique()
      dat4 <- dat2_reordered %>% select(bigtime, sourceNode, sourceN) %>% unique()
      dat4 <- dat4[1:3,]
      dat4$perce <- dat4$sourceN/sum(dat4$sourceN)
      dat3$perce <- unlist(tapply(dat3$targetN, dat3$targetTime, function(x) x/sum(x)))
      
      dat3 <- dat3 %>% mutate(Drug = substring(targetNode, 1,1)) %>% 
        select(targetTime, Drug, targetN, perce)
      dat4 <- dat4 %>% mutate(Drug = substring(sourceNode, 1,1)) %>%
        select(bigtime, Drug, sourceN, perce)
      names(dat3)[1] <- "bigtime"
      names(dat3)[3] <- names(dat4)[3] <- "Count"
      alldat <- rbind(dat4, dat3)
      
      # automatic color choices that are far away from each other
      paints <- brewer.pal(length(uniqueDrugs),"Set1")
      
      # for gg plot 
      
      nodestringlist <- paste(unlistwithin2$sourceNode,unlistwithin2$targetNode, collapse = ' ')
      
      # Split them up
      nodestringvector <- strsplit(nodestringlist, split =' ')
      
      # Find the unique nodes in order they appear
      node_order <- unique(nodestringvector[[1]])
      
      cols <- numeric(length(node_order));
      # find the drug name that they have
      # make sure the drug name length begins at the start 
      # of the nodes. 
      # for automatic creation of sankey plots, I think this step would 
      # have to be a separate data set 
      drugords <- substring(node_order, 1,1)
      
      for(i in 1:length(uniqueDrugs)){
        cols[drugords == uniqueDrugs[i]] <- paints[i[]]
      }
      drugcols <- cbind(cols, drugords)
      drugcols <- unique(drugcols)
      #cols[drugords == uniqueDrugs[1]] <- '#1A237E'
      #cols[drugords == "S"] <- '#FF6F00'
      #cols[drugords == "C"] <-  '#1B5E20'
      my_colors <- unique(cols)
      colurs <- cols; colurs <- factor(as.character(colurs))
      names(colurs) <- drugords
      
      #names(my_colors) <- levels(factor(c(levels(as.factor(alldat$Drug)), levels(as.factor(alldat$Drug)))))
      my_scale <- scale_fill_manual(name = "Drug", values = my_colors)   
      
      key.trans <- list(title=names(data_long2)[4],
                        space="top", columns=1, rows = 2, #2
                        text=list(drugcols[,2]),
                        lines=list(col= drugcols[,1]),
                        cex.title=1, cex=.9) 
      
      # for densities of age 
      ageDens <- lattice::densityplot(~Age|Time,
      par.settings = list(superpose.line = list(col = drugcols[,1])),
      groups = Drug, data = data_long2,layout = c(5,1), plot.points = F,
     key = key.trans, from = ageRange[1], to = ageRange[2],
     scales=list(y=list(rot=45), x=list(rot=45)))
      
      # frequency plots for ethnicity by time and drug
      tab4eth <- table(data_long2$ethnic, data_long2$Time,data_long2$Drug)
      ethnicHist <- lattice::barchart(tab4eth, groups = T,
                                      horizontal = F,
                                      par.settings = simpleTheme(col=drugcols[,1]),
                                      layout = c(5,1),
                                      auto.key= T,
           scales=list(y=list(rot=45), x=list(rot=45)),
           xlab = names(data_long2)[5], ylab = "Frequency")
      
      # frequency plots for network by time and drug
      tab4net <- table(data_long2$Network, data_long2$Time,data_long2$Drug)
      netHist <- lattice::barchart(tab4net, groups = T,
                                      horizontal = F,
                                      par.settings = simpleTheme(col=drugcols[,1]),
                                      layout = c(5,1),
                                      auto.key= T,
                                      scales=list(y=list(rot=45), x=list(rot=45)),
                                      xlab = names(data_long2)[2], ylab = "Frequency"
)
      
      
      # automatically makes colors for nodes in the order
      # provided by the data set 
      
      colsjson <- paste(cols, collapse= "', '")
      ncharpar <- nchar(colsjson)
      colsjson2 <- insert_str(colsjson[1], c("'", "'"), c(1, ncharpar+1)) 
      colsjson2 <- paste0("[",colsjson2,"]")
      
      
      #googleVis stuff
      begin <- "{link: { colorMode: 'gradient' }, node:{colors:"
      end <- ", label: { fontSize: 0.000001, bold: true}, nodePadding: 20, interactivity: true, labelPadding: 10}, iterations: 0}"
      
      forSankey <- paste0(begin, colsjson2, end)
      
      dat2_reordered <- dat2_reordered %>% mutate(
        brain.tooltip = ## Target nodes where 100% of patients came from same source:
          ## N + % (n) of source
          ifelse(targetNode %in% uniqueTarget,
                 paste0(to_from, '<br>N = ', Patients,
                        '<br>', round((Patients / sourceN) * 100),
                        '% of ', sourceN, ': ', sourceNode, ' &rArr; ',
                        '<br>', round((Patients / targetN) * 100),
                        '% of ', targetN, ': ', targetNode),
                 ## Source nodes where 100% of patients go to same target:
                 ## N + % (n) of target
                 ifelse(sourceNode %in% uniqueSource,
                        paste0(to_from, '<br>N = ', Patients, '<br>',
                               round((Patients / targetN) * 100), '% of ',
                               targetN, ': ', targetNode),
                        ## Otherwise, add N, % (n) of source, and % (n) of target
                        paste0(to_from, '<br>N = ', Patients,
                               '<br>', round((Patients / sourceN) * 100),
                               '% of ', sourceN, ': ', sourceNode, ' &rArr; ',
                               '<br>', round((Patients / targetN) * 100),
                               '% of ', targetN, ': ', targetNode)))
      )
      
      drugcols <- as.data.frame(drugcols); 
      names(drugcols) <- c("Color", "Drug")
      drugcols$Color <- as.character(drugcols$Color)
      dat2_reordered <- dat2_reordered %>% select(-c(bigtime, drugname))
      #alldat$perce.html.tooltip <- paste0('N = ',alldat$Count,'<br>',alldat$perce*100,'%')
      widedat <- reshape(alldat, timevar = "Drug", direction = "wide",
                         idvar = c("bigtime"), drop = c('perce'))
      list("my_scale" = my_scale, "alldat" = alldat, "SankeyDat" = dat2_reordered,
           "forSankey" = forSankey, "ageD" = ageDens, "ethChart" = ethnicHist, 
           "NChart" = netHist, "drugcols" = drugcols, "times" = times,
           "widedat" = widedat)
    })
    
    output$legend <- renderPlot({
    par(xpd = FALSE)
    plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=c(0, 1), ylim=c(0,1))
    legend(0, 1, legend=getsub()$drugcols[,2], fill=getsub()$drugcols[,1], title=names(getsub()$drugcols)[2],
           horiz = F, cex = 1.2, bty = "n", y.intersp = 2)
    }, width = 200, height = 500)
    
    output$timelabs <- renderUI({
    times <- paste(getsub()$times, collapse = "&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;")
    div(tags$div(HTML(times)))
    })
    #output$labels <- renderText({})
    output$stacked <- renderPlot({
      ggplot(getsub()$alldat, aes(fill=Drug, y=perce, x=bigtime)) + 
        geom_bar(position="fill", stat="identity",
                 width = .4) + ylab("Percent of Patients") +
        xlab("Time Point") +
        geom_text(aes(label = paste0(round(perce * 100), '% \n',
                                     "N = ", Count)),
                  position = position_stack(vjust = 0.5), color = "white") + 
        theme_classic() + getsub()$my_scale +
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=14,face="bold"),
              axis.title.x = element_text(size = 14, face= "bold"))
      
    }, height = 400, width = 730)
    
    output$barchart <- renderGvis({
      drug <- unique(getsub()$alldat$Drug)
      culs <- getsub()$drugcols[,1]; culs <- rev(culs);
      culs <- paste(culs, collapse= "', '")
      ncharpar <- nchar(culs)
      culs <- insert_str(culs[1], c("'", "'"), c(1, ncharpar+1)) 
      culs <- paste0("[",culs,"]")
      wides <- getsub()$widedat; colnames(wides)[-1] <- as.character(drug)
      wides[,2:ncol(wides)] <- wides[,ncol(wides):2]
      colnames(wides)[2:ncol(wides)] <- colnames(wides)[ncol(wides):2]
      gvisColumnChart(wides, xvar = "bigtime", yvar = colnames(wides)[-1],
                      options=list(tooltip="{isHtml:'true'}",
                                     isStacked = 'percent',
                                   width = 800,
                                   height = 400,
                                   legend = "{position: 'right',
                                   alignment: 'center'}",
                                   maxLines = 3,
                                   bar = "{groupWidth: '65%'}",
                                   colors = culs))
    })

    output$distPlot <- renderGvis({
        # generate bins based on input$bins from ui.R
        gvisSankey(getsub()$SankeyDat, from = "sourceNode",
                               to = "targetNode",
                               weight = "Patients",
                               options=list(height = 400, width = 550,
                                            tooltip = "{isHtml:'True'}",
                                            sankey = getsub()$forSankey))
      
    })
    
    output$ageD <- renderPlot({getsub()$ageD}, height = 400, width = 730)
    output$ethChart <- renderPlot({getsub()$ethChart}, height = 400, width = 730)
    output$NChart <- renderPlot({getsub()$NChart},height = 400, width = 730)
}


# Run the application 
shinyApp(ui = ui, server = server)
