# for kraken results dashboard
library(shiny)
library(shinydashboard)
library(DT)
library(sankeyD3)
library(magrittr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(reshape2)
library(reshape)
library(ggplot2)
library(ggsci)
library(Polychrome)
library(pheatmap)
library(RColorBrewer)
library(plotly)
library(magrittr)
library(networkD3)
library(pavian)
library(corrr)

kraken_classification_data<-read.delim("/home/ibab/bigyani/4th_sem/project/dashboard/kraken_classification.tsv", sep ="\t")
colnames(kraken_classification_data)[5:10] <- c('TI','TII','TIII','TIV', 'BW','MW')
kraken_classification_data[is.na(kraken_classification_data)] <- 0
my_colors <- c("#f8c607","#1c6b1a","#cc5c00","#b695f6","#ff5e5e","#249fc3","#a32161","#42e58f","#c57c80","#fd6c9e",
                        "#f25512","#870a0a","#84aa09","#11ff11","#ff1111","#9f5084","#ffb7a1","#703815",
                        "#6f787f","#fe54ff","#b6873f","#61f1fd","#1111ff","#21a363","#e5d625","#6321a3","#90d1b1")
                        


ui<- dashboardPage(
  dashboardHeader(title =" An. stephensi metagenome"),
  dashboardSidebar(sidebarMenu(
    menuItem("Homepage",tabName = "homepage", icon=icon("building", class = NULL, lib="font-awesome")),
    menuItem("Visualization", tabName = "visualization", icon = icon("bar-chart")),
    menuItem("Analysis", tabName = "analysis", icon = icon("table"),
     menuSubItem("Rank based correlation plot",tabName = "correlationplot"),
     menuSubItem("Taxa bundance across samples ",tabName = "lineplot"))
  
  )),
  dashboardBody(
    fluidPage(tags$head(
      tags$style(
        HTML("#Plot_line_graph {
          background-color: #61f1fd;
          width: 100px;
          border-width: 3px;}"))),
      tabItems(
      # tabItem(tabName = "analysis", 
      #         h1("Analysis"),
      # titlePanel("DT table for kraken results"),
      #   fluidRow(
      # box(width= 6,DTOutput("kraken_table")),
      # box(width= 6,DTOutput("kraken_table2"))
      # )
   # ),
    # tabItem(tabName = "topspecies", 
    #         h1("The top ten species in samples"),
    #         fluidRow(
    #           plotlyOutput("top_ten_species", height=800)
    #         )
    # ),
    # tabItem(tabName = "sankeyplot", 
    #         h1("Sankey Plot"),
    #         fluidRow(
    #           box(width=30, sankeyNetworkOutput("sankey_plot", height=500))
    #         )
    # )
    tabItem(tabName="visualization", 
            fluidRow(
              box(width= 12, DTOutput("kraken_table",height=400)),
              box(width=9, sankeyNetworkOutput("sankey_plot", height=500)),
              box(width=3, plotlyOutput("top_ten_species", height=500))
            )),
    tabItem(tabName = "lineplot", 
            fluidRow(
            column(4, selectizeInput("taxa", "Choose a taxa:",list("Genus", "Order"))),
            column(width=6, 
                   selectizeInput(inputId = "taxaSelected",label = "Taxaname", choices = NULL, multiple=TRUE)),
                    actionButton(inputId = "Plot_line_graph", label = "PLOT"),
            box(width= 12, DTOutput("taxaranktable"),height=300),
            box(width=12, plotlyOutput("across_taxa_plot"),height=400))),
    tabItem(tabName = "correlationplot",
            column(6, selectizeInput("sample", " Choose a sample population: ",list("TI", "TII", "TIII", "TIV", "BW", "MW"))),
            column(6, selectizeInput("taxon", "Choose a taxonomic rank: ",list("Species", "Genus", "Family", "Class", "Order"))),
            box(width=6, plotlyOutput("top_species_across_samples"), height=500),
            box(width=6,plotOutput("correlation_plot"), height=500))
    )
  )
)
)
server <- function(input, output, session) {
  kraken_data <- read.delim("/home/ibab/bigyani/4th_sem/project/dashboard/kraken_classification.tsv",sep="\t")
  output$kraken_table <- renderDT({
     colnames(kraken_data)[5:10] <- c("TI","TII","TIII","TIV","BW","MW") 
    data<- datatable(kraken_data,
                            options = list(paging = TRUE,    
                                           pageLength = 5,  
                                           scrollX = TRUE,   
                                           scrollY = TRUE,  
                                           Width = 3,
                                           height =3,   
                                           server = FALSE,
                                           dom = 'Bfrtip',
                                           buttons = c('csv', 'excel')
                            ),
                     extensions = 'Buttons',
                     selection = 'single', 
                     filter = 'top',
                     rownames = FALSE  
                                        
    ) %>% formatStyle(c('TI','TII','TIII','TIV','BW','MW'),
                     background = styleColorBar(c(kraken_data$TI, kraken_data$TII, kraken_data$TIII, kraken_data$TIV, kraken_data$BW, kraken_data$MW), 'AQUAMARINE'),
                     backgroundSize = '98% 88%',
                     backgroundRepeat = 'no-repeat',
                     backgroundPosition = 'center')
  }
  )
    
    output$kraken_table2 <- renderDT({
      kraken_data2=data.frame()
      for(i in 1:nrow(kraken_data))
      {
        if(kraken_data[i,2] =='S')
        {
         kraken_data2 <- rbind(kraken_data2, kraken_data[i,])
        }
      }
     data2<- datatable(kraken_data2[,1:2],
                     options = list(paging = TRUE,    
                                    pageLength = 6,  
                                    scrollX = TRUE,   
                                    scrollY = TRUE,  
                                    Width = 3,
                                    height =3,   
                                    server = FALSE
                     ),
                     selection = 'single', 
                     rownames = FALSE  
                     
    )
    })
    output$top_ten_species <- renderPlotly({

      kraken_data_species <- read.delim("/home/ibab/bigyani/4th_sem/project/dashboard/kraken_all.tsv",sep="\t")
      kraken_data2=data.frame()
      for(i in 1:nrow(kraken_data_species))
      {
        if(kraken_data_species[i,2] =='S')
        {
          kraken_data2 <- rbind(kraken_data2, kraken_data_species[i,])
        }
      }
      
      TI=data.frame()
      TI <- kraken_data2[,c(1,5)] %>% arrange(desc(kraken_data2[5])) %>% 
        slice_head(n=10) 
      TII=data.frame()
      TII <- kraken_data2[,c(1,6)] %>% arrange(desc(kraken_data2[6])) %>% 
        slice_head(n=10)
      TIII=data.frame()
      TIII <- kraken_data2[,c(1,7)] %>% arrange(desc(kraken_data2[7])) %>% 
        slice_head(n=10)
      TIV=data.frame()
      TIV <- kraken_data2[,c(1,8)] %>% arrange(desc(kraken_data2[8])) %>% 
        slice_head(n=10)
      BW=data.frame()
      BW <- kraken_data2[,c(1,9)] %>% arrange(desc(kraken_data2[9])) %>% 
        slice_head(n=10)
      
      MW=data.frame()
      MW <- kraken_data2[,c(1,10)] %>% arrange(desc(kraken_data2[10])) %>% 
        slice_head(n=10)
      
      sample_dataframe <-list(TI,TII,TIII,TIV,BW,MW)
      top_10_species_all_sample_df<-sample_dataframe %>% reduce(full_join, by="name")
      species_all_sample_df<-cbind(rapply(top_10_species_all_sample_df, f=function(x) ifelse(is.na(x),0,x), how="replace" ))
      colnames(species_all_sample_df)[2:7]<- c("TI","TII","TIII","TIV","BW","MW")
      
      species_data_percentage <- apply(as.data.frame(sapply(species_all_sample_df[,-1], as.numeric)), 2, 
                                       function(x){x*100/sum(x,na.rm=T)})
      rownames(species_data_percentage) <-species_all_sample_df[,1]
      
      
      data = melt(species_data_percentage[,c(1:6)])
      data$Var2 <- factor(data$Var2, levels=c("TI","TII","TIII","TIV","BW","MW"))
      species_abundance_plot<-ggplot(data=data, aes(x = Var2, y = value, fill = Var1)) +  geom_bar(stat = "identity") + 
        theme(legend.position = "none")+ scale_fill_manual(values=my_colors)+
        #theme(axis.text.x=element_text(angle = 90))   +  theme(legend.position="right", legend.text = element_text(size=13)) + 
        labs(fill = "Species") + ggtitle("India(pooled)species level-(Top species)")+
        theme(plot.title=element_text(size=9))+
        xlab("Population") + ylab("Top Ten Species")
      ggplotly(species_abundance_plot)
    })
    output$sankey_plot <- renderSankeyNetwork({
     
      report <- read_report("/home/ibab/bigyani/4th_sem/project/dashboard/TI_merged_blast_no_param_kraken.kreport")
      
      build_sankey_network <- function(my_report, taxRanks =  c("D","K","P","C","O","F","G","S"), maxn,
                                       zoom = F, title = NULL,
                                       ...) {
        stopifnot("taxRank" %in% colnames(my_report))
        if (!any(taxRanks %in% my_report$taxRank)) {
          warning("report does not contain any of the taxRanks - skipping it")
          return()
        }
        my_report <- subset(my_report, taxRank %in% taxRanks)
        my_report <- plyr::ddply(my_report, "taxRank", function(x) x[utils::tail(order(x$cladeReads,-x$depth), n=maxn), , drop = FALSE])
        
        my_report <- my_report[, c("name","taxLineage","taxonReads", "cladeReads","depth", "taxRank")]
        
        my_report <- my_report[!my_report$name %in% c('-_root'), ]
        #my_report$name <- sub("^-_root.", "", my_report$name)
        
        splits <- strsplit(my_report$taxLineage, "\\|")
        
        ## for the root nodes, we'll have to add an 'other' link to account for all cladeReads
        root_nodes <- sapply(splits[sapply(splits, length) ==2], function(x) x[2])
        
        sel <- sapply(splits, length) >= 3
        splits <- splits[sel]
        
        links <- data.frame(do.call(rbind,
                                    lapply(splits, function(x) utils::tail(x[x %in% my_report$name], n=2))), stringsAsFactors = FALSE)
        colnames(links) <- c("source","target")
        links$value <- my_report[sel,"cladeReads"]
        
        my_taxRanks <- taxRanks[taxRanks %in% my_report$taxRank]
        taxRank_to_depth <- stats::setNames(seq_along(my_taxRanks)-1, my_taxRanks)
        
        
        nodes <- data.frame(name=my_report$name,
                            depth=taxRank_to_depth[my_report$taxRank],
                            value=my_report$cladeReads,
                            stringsAsFactors=FALSE)
        
        for (node_name in root_nodes) {
          diff_sum_vs_all <- my_report[my_report$name == node_name, "cladeReads"] - sum(links$value[links$source == node_name])
          if (diff_sum_vs_all > 0) {
            nname <- paste("other", sub("^._","",node_name))
            #nname <- node_name
            #links <- rbind(links, data.frame(source=node_name, target=nname, value=diff_sum_vs_all, stringsAsFactors = FALSE))
            #nodes <- rbind(nodes, nname)
          }
        }
        
        names_id = stats::setNames(seq_len(nrow(nodes)) - 1, nodes[,1])
        links$source <- names_id[links$source]
        links$target <- names_id[links$target]
        links <- links[links$source != links$target, ]
        
        nodes$name <- sub("^._","", nodes$name)
        links$source_name <- nodes$name[links$source + 1]
        links$link_color_type <- sub(' .*', '',nodes[links$source + 1, 'name'])
        
        if (!is.null(links))
          sankeyD3::sankeyNetwork(
            Links = links,
            Nodes = nodes,
            doubleclickTogglesChildren = TRUE,
            Source = "source",
            Target = "target",
            Value = "value",
            NodeID = "name",
            NodeGroup = "name",
            NodePosX = "depth",
            NodeValue = "value",
            LinkGroup = "link_color_type",
            dragY = TRUE,
            dragX = TRUE,
            xAxisDomain = my_taxRanks,
            numberFormat = "pavian",
            title = title,
            nodeWidth = 5,
            linkGradient = TRUE,
            nodeShadow = TRUE,
            nodeCornerRadius = 2,
            units = "cladeReads",
            fontSize = 11,
            iterations =  1,
            align = "none",
            highlightChildLinks = TRUE,
            orderByPath = TRUE,
            scaleNodeBreadthsByString = TRUE,
            zoom = zoom,
            ...
          )
      }
      build_sankey_network(report, maxn=15)
    }
    )
    rv <- reactiveValues(filtered_data = NULL, kraken_classification_data2 = NULL)
    
    observeEvent(input$taxa,{
      req(input$taxa)
      if(input$taxa == 'Order') {
        rv$kraken_classification_data2 = kraken_classification_data[(kraken_classification_data$taxRank == "O"),]
      } else if (input$taxa == 'Genus'){
        rv$kraken_classification_data2 = kraken_classification_data[(kraken_classification_data$taxRank == "G"),]
      }
      updateSelectizeInput(session, "taxaSelected", choices = rv$kraken_classification_data2$name)
      output$taxaranktable <- renderDT({ data2<- datatable(rv$kraken_classification_data2,
                                                           options = list(paging = TRUE, pageLength = 6, scrollX = TRUE,scrollY = TRUE, Width = 3,height =3,server = FALSE
                                                           ), selection = 'single', rownames = FALSE)
      })
    })
    
    observeEvent(input$Plot_line_graph, {
      
      variable <- rv$kraken_classification_data2[grepl(paste(input$taxaSelected, collapse="|"), rv$kraken_classification_data2$name, ignore.case = TRUE), ]
      variable <- variable[, c("name", "TI", "TII", "TIII", "TIV", "BW", "MW")]
      melted_data <- reshape2::melt(variable, id.vars = "name")
      
      
      output$across_taxa_plot <- renderPlotly({
        p <- ggplot(data=melted_data, aes(x = variable, y = value, group=name, color=name)) +
          geom_line() + ggtitle(" Read Count percentage for chosen organism")+
          labs(y="percentage", x = "Sample")+
          theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
          scale_color_discrete(labels = element_text(size = 4))
        ggplotly(p)
      })
    })
    observeEvent(list(input$taxon, input$sample), {
      req(input$taxon)
      # browser()
      tax_rank <- substr(input$taxon,1,1)
        all_population_rank <- kraken_classification_data %>%
          filter(taxRank == tax_rank) %>%
          select(5:10) %>%
          replace(is.na(.), 0)
        colnames(all_population_rank) <-  c('TI','TII','TIII','TIV','BW','MW')
        lab_res_cor_rank <- cor(all_population_rank, method = "spearman")
        pheatmap(lab_res_cor_rank,color = RColorBrewer::brewer.pal(9, "Reds"),display_numbers = F,number_color="black", fontsize_number = 12, main= "SPEARMAN RANK BASED BACTERIAL CORRELATION- Kraken")
      
         output$correlation_plot <- renderPlot({
         Plot <- network_plot(correlate(all_population_rank, use = "complete.obs" ,method = "kendall" ), min_cor=0.01, colors = c("red" ,"blue"))
          main_plot <- Plot + labs(title=paste(input$taxon, " based Kendall Rank Correlation for all samples - Network Plot", sep =" "))+ 
          theme(plot.title = element_text(size=11, face ="bold"))
          main_plot
    })
       req(input$sample)
       abundant_taxa <- kraken_classification_data %>% filter(taxRank == tax_rank) %>% 
                        select(name,taxRank, input$sample) %>%  arrange(desc(.data[[input$sample]])) %>% head(5)
       output$top_species_across_samples <- renderPlotly({
       plot <- ggplot(data = abundant_taxa, aes(x=name, y=.data[[input$sample]], fill=name))+geom_bar(stat = "identity") + 
         theme(legend.position = "none")+ scale_fill_manual(values=my_colors)+
         scale_x_discrete(label = function(x) stringr::str_trunc(x, 12)) +
         theme(axis.text.x = element_text(angle = 60, hjust = 1.2, vjust = 0.2))+
         ggtitle(paste("Top organisms for chosen taxa and sample ", {{input$sample}}, sep= " "))+ 
         labs(fill = "Species") + theme(plot.title=element_text(size=12, face="bold"))+xlab("species")+ ylab("percentage abundance")
       ggplotly(plot)
    })
    })
   
    
}                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 

shinyApp(ui, server)


