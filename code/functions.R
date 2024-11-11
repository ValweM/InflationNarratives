read_DJ <- function(file) {
  
  ## create path
  path <- paste0("../data/data/", file)
  
  ## read in file
  lines   <- readLines(path) 
  save(lines, file = str_remove(paste0("../data/rds/", file, ".rds"), "\\.nml\\.gz"))
  rm(lines)
  gc()
}


myplot <- function(results_lin) {
  irf_lin_mean <- results_lin[[1]]
  irf_lin_low <- results_lin[[2]]
  irf_lin_up <- results_lin[[3]]
  
  specs <- results_lin$specs
  
  # Plots for lin function
  if (specs$model_type == 0) {
    plot_num <- 1
    gg_lin <- rep(list(NaN), specs$endog * specs$endog)
    
    # Loop to fill to create plots
    for (rr in 1:(specs$endog)) {
      for (ss in 1:(specs$endog)) {
        # Tibbles for linear irfS
        tbl_lin_mean <- as.matrix(t(irf_lin_mean[, 1:specs$hor, ss]))[, rr]
        tbl_lin_low <- as.matrix(t(irf_lin_low[, 1:specs$hor, ss]))[, rr]
        tbl_lin_up <- as.matrix(t(irf_lin_up[, 1:specs$hor, ss]))[, rr]
        
        tbl_lin <- tibble(
          x = seq_along(tbl_lin_mean),
          mean = tbl_lin_mean,
          low = tbl_lin_low,
          up = tbl_lin_up
        )
        
        gg_lin[[plot_num]] <- ggplot() +
          geom_line(data = tbl_lin, aes(y = mean, x = x), size = 1) +
          geom_ribbon(data = tbl_lin, aes(x = x, ymin = low, ymax = up), col = 'grey',
                      fill = 'grey', alpha = 0.3) +
          theme_minimal() +
          xlab('Horizon in Months') +
          ylab('Change in Variable') +
          theme(
            title = element_blank(),
            plot.title = element_blank(),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            legend.position = "none"
          ) +
          scale_y_continuous(expand = c(0, 0)) +
          scale_x_continuous(
            expand = c(0, 0),
            breaks = seq(0, specs$hor, 2)
          ) +
          geom_hline(yintercept = 0, col = "black", linetype = "dashed")
        
        # Add one to count variable
        plot_num <- plot_num + 1
      }
    }
  } else if (specs$model_type == 1 | specs$model_type == 2) {
    gg_lin <- rep(list(NaN), specs$endog)
    
    # Loop to fill to create plots
    for (rr in 1:(specs$endog)) {
      # Tibbles for linear irfS
      tbl_lin_mean <- irf_lin_mean[rr, ]
      tbl_lin_low <- irf_lin_low[rr, ]
      tbl_lin_up <- irf_lin_up[rr, ]
      
      tbl_lin <- tibble(
        x = seq_along(tbl_lin_mean),
        mean = tbl_lin_mean,
        low = tbl_lin_low,
        up = tbl_lin_up
      )
      
      gg_lin[[rr]] <- ggplot() +
        geom_line(data = tbl_lin, aes(y = mean, x = x), size = 1) +
        geom_ribbon(data = tbl_lin, aes(x = x, ymin = low, ymax = up), col = 'grey',
                    fill = 'grey', alpha = 0.3) +
        theme_minimal() +
        xlab('Horizon in Months') +
        ylab('Change in Variable') +
        theme(
          title = element_blank(),
          plot.title = element_blank(),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          legend.position = "none"
        ) +
        scale_y_continuous(expand = c(0, 0)) +
        scale_x_continuous(
          expand = c(0, 0),
          breaks = seq(0, specs$hor, 2)
        ) +
        geom_hline(yintercept = 0, col = "black", linetype = "dashed")
    }
  }
  
  return(gg_lin)
}


lp_plot <- plot_lin <- function(results_lin){
  
  
  irf_lin_mean <- results_lin[[1]]
  irf_lin_low  <- results_lin[[2]]
  irf_lin_up   <- results_lin[[3]]
  
  
  specs        <- results_lin$specs
  
  # Plots for lin function
  if(specs$model_type == 0){
    
    plot_num     <- 1
    gg_lin       <- rep(list(NaN), specs$endog*specs$endog)
    
    # Loop to fill to create plots
    for(rr in 1:(specs$endog)){
      for (ss in 1:(specs$endog)){
        
        # Tibbles for linear irfS
        tbl_lin_mean <- as.matrix(t(irf_lin_mean[, 1:specs$hor , ss]))[, rr]
        tbl_lin_low  <- as.matrix(t(irf_lin_low[,  1:specs$hor , ss]))[, rr]
        tbl_lin_up   <- as.matrix(t(irf_lin_up[,   1:specs$hor , ss]))[, rr]
        
        tbl_lin      <- tibble(x     = seq_along(tbl_lin_mean),  mean = tbl_lin_mean,
                               low   = tbl_lin_low, up = tbl_lin_up)
        
        gg_lin[[plot_num]] <- ggplot()+
          geom_line(data     = tbl_lin, aes(y = mean, x = x)) +
          geom_ribbon(data   = tbl_lin, aes(x = x, ymin = low, ymax = up), col = 'grey',
                      fill   = 'grey', alpha  = 0.3) +
          theme_classic() +
          ggtitle(paste(specs$column_names[ss], 'on', specs$column_names[rr], sep=" ")) +
          xlab('') +
          ylab('') +
          theme(title = element_text(size = 20),
                plot.title = element_text(hjust = 0.5),
                axis.text.x=element_text(size=15),
                axis.text.y=element_text(size=15)) +
          scale_y_continuous(expand = c(0, 0))          +
          scale_x_continuous(expand = c(0, 0),
                             breaks = seq(0, specs$hor, 2))  +
          geom_hline(yintercept = 0, col = "black", linewidth = 1, linetype = "dashed")
        
        # Add one to count variable
        plot_num     <- plot_num + 1
        
      }
    }
    
  } else if(specs$model_type == 1| specs$model_type == 2){
    
    gg_lin       <- rep(list(NaN), specs$endog)
    
    # Loop to fill to create plots
    for(rr in 1:(specs$endog)){
      
      # Tibbles for linear irfS
      tbl_lin_mean <- irf_lin_mean[rr, ]
      tbl_lin_low  <- irf_lin_low[rr, ]
      tbl_lin_up   <- irf_lin_up[rr, ]
      
      tbl_lin      <- tibble(x     = seq_along(tbl_lin_mean),  mean = tbl_lin_mean,     # 1:(specs$hor)
                             low   = tbl_lin_low,    up   = tbl_lin_up)
      
      gg_lin[[rr]] <- ggplot()+
        geom_line(data     = tbl_lin, aes(y = mean, x = x)) +
        geom_ribbon(data   = tbl_lin, aes(x = x, ymin = low, ymax = up), col = 'grey',
                    fill   = 'grey', alpha  = 0.3) +
        theme_classic() +
        ggtitle(paste('Shock', 'on', specs$column_names[rr], sep=" ")) +
        xlab('') +
        ylab('') +
        theme(title = element_text(size = 6),
              plot.title = element_text(hjust = 0.5)) +
        scale_y_continuous(expand = c(0, 0))          +
        scale_x_continuous(expand = c(0, 0),
                           breaks = seq(0, specs$hor, 2))  +
        geom_hline(yintercept = 0, col = "black", linewidth = 0.25, linetype = "dashed")
      
      
      
    }
  }
  
  return(gg_lin)
  
}




devide_files <- function(line) {
  
  path <- paste0("../data/dfs/", line)
  load(path)
  ## define start and end of xml files
  start   <- grep('<?xml version="1.0"',lines,fixed=T)  ## <?xml version="1.0" encoding="ISO-8859-1" ?> not always the same
  end     <- c(start[-1]-1,length(lines))
  
  devide <- round(length(start)/4)
  
  split_line <- lines[1:end[devide]]
  save(split_line, file = paste0(str_remove(paste0("../data/rdata/", line), "\\.RData"), "_01.rds"))
  
  split_line <- lines[start[devide]:end[(2*devide)]]
  save(split_line, file = paste0(str_remove(paste0("../data/rdata/", line), "\\.RData"), "_02.rds"))
  
  split_line <- lines[start[(2*devide)]:end[(3*devide)]]
  save(split_line, file = paste0(str_remove(paste0("../data/rdata/", line), "\\.RData"), "_03.rds"))
  
  split_line <- lines[start[(3*devide)]:tail(end, 1)]
  save(split_line, file = paste0(str_remove(paste0("../data/rdata/", line), "\\.RData"), "_04.rds"))
  
  rm(split_line)
  gc()
}




get_DJ <- function(line) {
  
  path <- paste0("../data/rds/", line)
  

  load(path)
  
  ## define start and end of xml files
  start   <- grep('<?xml version="1.0"',lines,fixed=T)  ## <?xml version="1.0" encoding="ISO-8859-1" ?> not always the same
  end     <- c(start[-1]-1,length(lines))
  

 lines <-lines %>% 
    str_replace_all("&", "&amp;") %>%
    str_remove_all("[^\u0009\u000a\u000d\u0020-\uD7FF\uE000-\uFFFD]") %>%
    str_remove_all("[^\u0009\u000d\u0020-\uD7FF\uE000-\uFFFD]") %>%
    str_remove_all("[^\u0009\r\n\u0020-\uD7FF\uE000-\uFFFD\ud800\udc00-\udbff\udfff]")
 
     get.xml <- function(i) {
      txt <- paste(lines[start[i]:end[i]],collapse="\n") 
     
      xmlParse(txt,asText=TRUE, encoding="utf-8")  
    }
    

  
  docs <- lapply(1:length(start),get.xml)
 
  rm(lines)
  gc()
  
  

  ## get Attributes 
  
  DJ <- vector("list", length(docs))
  
  
  for(i in seq(1, length(docs))) {
    DJ[[i]] <-  sapply(c("docdate", "seq", "news-source"), function(x) xpathSApply(docs[[i]], c('//doc/djnml', "//docdata/djn/djn-newswires") , xmlGetAttr, x))
    
  
  }
  
  for (i in seq(1, length(DJ))) {
    
    DJ[[i]][1, "news-source"] <- DJ[[i]][2,"news-source"]
    DJ[[i]] <- DJ[[i]][-2, ]
  }
  
  DJ <- bind_rows(DJ,  .id = "column_label")
 
  ## combine data
  
  DJ <- DJ %>%
    as.data.table() %>%
    add_column("headline" = NA) %>%
    add_column("text" = NA) %>%
    add_column("subject" = NA)
  
  ## for loop for headline, text and subject
  for (i in seq(1, length(docs)))  {
   
    DJ$headline[i] <- docs[[i]] %>% xmlToDataFrame(nodes = getNodeSet(., "//body/headline")) %>% .$text
    DJ$text[i] <- docs[[i]] %>% xmlToDataFrame(nodes = getNodeSet(., "//body")) %>% .$text
    DJ$subject[i] <- docs[[i]] %>% xmlToDataFrame(nodes=getNodeSet(., "//djn-coding")) %>% .$"djn-subject"
  }
  
  ## Generate Doc ID
  ## Remove duplicates if text is same
  
  DJ <- DJ[!duplicated(DJ$text),]
  
  DJ <- mutate(DJ, ID = paste0(seq, docdate))
  

  
}
