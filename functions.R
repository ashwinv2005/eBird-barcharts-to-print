plotbarchart = function(n.spec.page, dat, indialist, region)
{
  require(splitstackshape)
  require(magrittr)
  require(dplyr)
  require(tidyverse)
  require(reshape2)
  
  #listmap <- read.delim2("Checklist Mapper.csv", sep=',')
  
  dat = dat %>%
    filter(SCIENTIFIC.NAME %in% indialist$SCIENTIFIC.NAME)
  
  # Extract the species list
  species_list <- as.data.frame (cbind (dat$COMMON.NAME, dat$SCIENTIFIC.NAME))
  colnames(species_list) <- c("COMMON.NAME", "SCIENTIFIC.NAME")
  
  # Remove all non-data columns
  dat <- dat[,-c(49,50,51)]
  
  # Make all of them numeric for cut
  
  dat <- as.data.frame (lapply(dat, as.numeric))
  
  # eBird uses the same breaks
  my_breaks <- c(0.0, 0.0000000001, 0.0025, 0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.6, 1)
  my_labels <- c(NA, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  
  dat <- mapply (cut, dat, MoreArgs = list (breaks =my_breaks,
                                            labels =my_labels,
                                            include.lowest = TRUE))
  

  l = length(species_list$COMMON.NAME)
  p = n.spec.page
  totp = ceiling(l/p)
  ext = p*totp-l
  temp = as.matrix(dat[(1):(ext),])
  if (ext == 1)
  {
    temp = t(temp)
  }
  if (ext > 0)
  {
    temp[,] = NA
    dat = rbind(dat,temp)
  }
  

  for (i in 1:ext)
  {
    a = rep(" ",i)
    a = paste(a, collapse = '')
    if (i == 1)
    {
      st = a
    }
    if (i > 1)
    {
      st = c(st,a)
    }
  }
  
  
  # Assign row names
  row.names(dat) <- c(as.character(species_list$COMMON.NAME),as.character(st))
  colnames(dat) <- c(1:48) 
  
  
  
  saveRDS(dat, paste(region,"_barchart.rds",sep=""))
  
  plotdata <- melt(dat)
  colnames(plotdata) <- c("COMMON.NAME","WEEK", "ABUNDANCE")
  plotdata$ABUNDANCE <- as.numeric(plotdata$ABUNDANCE)
  
  getMonth <- function (week)
  {
    Months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
    return ( Months [as.integer(1 + ((week-1)/4))])
    
  }
  
  plotdata <- within (plotdata, MONTH <-  getMonth(WEEK))
  
  speclist = as.character(unique(plotdata$COMMON.NAME))
  pdf(paste(region,"_barchart.pdf",sep=""))
  
  for (i in 1:totp)
  {
    species = speclist[(p*(i-1)+1):(p*i)]
    barchart_plot = barchart(plotdata,species)
    # Print the ggplot to the current page
    print(barchart_plot)
  }
  
  # Close the PDF device
  dev.off()
}



barchart = function(new,species)
{
  #  image <- png::readPNG("Months.png")
  require(grid)
  require(tidyverse)
  require(ggthemes)
  theme_set(theme_tufte())
  
  new1 = new
  new1$ABUNDANCE = -1*new1$ABUNDANCE
  toplot = rbind(new,new1)
  toplot = toplot[toplot$COMMON.NAME %in% species,]
  toplot$MONTH = factor(toplot$MONTH, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  l = length(species)
  if (l %% 2 == 0)
  {
    alt = data.frame(COMMON.NAME = species,col = rep(c("grey","white"),l/2))
  }
  if (l %% 2 == 1)
  {
    alt = data.frame(COMMON.NAME = species,col = c(rep(c("grey","white"),(l-1)/2),"grey"))
  }
  
  toplot = left_join(toplot,alt)
  toplot$COMMON.NAME = factor(toplot$COMMON.NAME, levels = species)
  
  
  ggp = ggplot(toplot, aes(x=WEEK, y=ABUNDANCE)) + 
    geom_rect(aes(fill = col),xmin = -Inf,xmax = Inf,
              ymin = -Inf,ymax = Inf,alpha = 0.1) +
    facet_grid(COMMON.NAME~MONTH, 
               scale = "free_x", switch = "y", labeller = label_wrap_gen()) +
    geom_bar(stat = "identity", position = "identity", fill = "dark green") +
    xlab("MONTH") +
    ylab("COMMON.NAME")
  
  ggp1 = ggp +
    ## use element_text to retain axis labels/titles/ticks with appropriate sizes specified
    theme(axis.title.x = element_blank(),
          axis.text.x  = element_blank(), 
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(), 
          axis.text.y  = element_blank(), 
          axis.ticks.y = element_blank(),
          strip.background.x = element_rect(color = "black", linewidth = 1),
          #strip.text.x = element_blank(),
          #strip.text.y = element_blank()) +
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.text.y.left = element_text(size = 8, face = "bold", angle = 0)) +
    theme(legend.position="none") +
    scale_y_continuous(limits = c(-10,10)) +
    scale_fill_manual(values = c("grey","white"), breaks = c("grey","white"))
  
  return(ggp1)
}
