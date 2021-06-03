function(xrange=50, yrange=50, adjust=0, threshold=0.5, cex=1){
  
  library(Momocs)
  library(tidyverse)
  library(foreach)
  library(doParallel)
  library(jpeg)
  library(imager)
  #setwd("C:/Users/ageffen/Dropbox/Cefas/working_folder/New_folder/Otolith_outline_extractionV1")
  w.dir = getwd()
  
  fix.image.path = paste(w.dir,"Images/",sep="/")
  setwd(fix.image.path)
  
  # Select pictures for shape extraction 
  jpg.list.all<-list.files(pattern = ".jpg")
  jpg.list.all
  
  if(exists("oto")==FALSE){
    oto<-list()
  }
  
  total=0
  total.images=0
  fnames = list.files(pattern="\\.(jpg|jpeg)",ignore.case=T)
  for (fname in fnames)
  {
    strip.fname = gsub(".(jpg|jpeg)","",fname,ignore.case=T) 
    total.images = total.images+1                
    if((!(strip.fname %in% names(oto)) ))
    {
      total=total+1 
    }
  }
  
  if(total==0)
  {
    message(paste("All" ,total.images, "otolith outlines have been detected."))
    return(oto)
  }
  
  jpg.list <- jpg.list.all[!jpg.list.all%in%paste(names(oto), ".jpg", sep="")]
  
  ### I. 1st step: Binarization 
  
  cl <- makeCluster(detectCores())      #Creates parallel workers for faster computing. Useful if many images.
  registerDoParallel(cl)
  cl
  
  
  ### This loop uses "Imager" to prepare without using an external software. It first converts all images to grayscale. 
  ### Then, it draws a white rectangle in the middle to help the outline detection that can sometimes fail if there
  ### are darker spots on the surface of the otolith. Then it inverses the pixel values and seclect the thresholding 
  ### between pure white and pure black. Binary images are saved in the associated sub-folder.
  
  write.binary.path = paste(fix.image.path,"Binary/",sep="/")
  if(!file.exists(write.binary.path)){
    suppressWarnings(dir.create(write.binary.path))
  }
  
  foreach(i=1:length(jpg.list),.errorhandling="pass", .packages = c("doParallel",  "imager"))%dopar%{
    
    a <- jpg.list[[i]]%>%load.image()
    im_width <- a %>% width()
    im_height <- a %>% height()
    
    a <- a%>%grayscale()%>%
      draw_rect( im_width/2-xrange, im_height/2-yrange, im_width/2+xrange, im_height/2+yrange, color = 1, opacity = 1, filled = TRUE) %>% 
      imchange(where = ~xs>0,fo = ~1-. )%>%#inverse color
      threshold("auto", approx = T, adjust = 1.4+adjust)    # important! test adjust level
    a %>%as.cimg%>% imager::save.image( file = paste(write.binary.path,jpg.list[[i]], sep="")) 
  }
  message("1. All binary pictures are produced!")
  
  
  ### II. 2nd step: Outline extraction 
  
  oto.new<-foreach(i=paste("Binary/",jpg.list, sep=""),.errorhandling="pass", .packages = c("doParallel", "Momocs"))%dopar%{
    withCallingHandlers({ 
      setTimeLimit(10)
      import_jpg(i, threshold = 2500)  #threshold value is also important here
    }, 
    error = function(e) {
      ###  error handling
    }
    )
  }
  message("2. All otolith outlines are extracted!")
  
  ### Verify that all outlines were properly extracted
  oto.new <- do.call(c, oto.new)    
  
  
  # Non-errors
  noerror <- names(oto.new)[!names(oto.new)%in%c("message", "call")]
  #npoints <- sapply(oto.new, function (x) dim(x%>%as.data.frame()))[1,] 
  npoints <- unlist(lapply(oto.new, FUN = function (x) dim(x)[[1]]))
  npoints <- names(npoints[npoints<500]) # shows number of points  along the otolith contour 
  noerror <- noerror[!noerror%in%npoints]
  noerror
  
  # Errors:
  errors <- jpg.list[!jpg.list%in%paste(noerror, ".jpg", sep="")]     # should show 0
  errors
  
  
  ### III. 3rd step: Superimposition of outlines on the original picture to check their reliability
  
  jpgnames<-paste(names(oto.new), ".jpg", sep="")
  
  ### This loop loads each otolith picture, extracts the coordinates of each outline generated before and  
  ### superimposes them on top of another. Because the internal plot coordinates are different, we define
  ### X and Y coordinates for the outline so that it is aligned with the original picture.
  
  write.superimposed.path = paste(fix.image.path,"Superimposed/",sep="/")
  if(!file.exists(write.superimposed.path)){
    suppressWarnings(dir.create(write.superimposed.path))
  }
  
  foreach(i=1:length(jpgnames),.errorhandling="pass", .packages = c("jpeg","doParallel", "imager","tidyverse"))%dopar%{
    img <- readJPEG(jpgnames[[i]], native = TRUE)
    im_width <- img %>% width()
    im_height <- img %>% height()
    otoname <- substr(jpgnames[i],1,nchar(jpgnames[i])-4)
    pathi <-  paste(write.superimposed.path,otoname,".jpg", sep="")
    x<-(oto.new[[otoname]]%>%as_tibble())$V1/(dim(img)[2])+1#x
    y<-(oto.new[[otoname]]%>%as_tibble())$V2/(dim(img)[1])+1#y
    #  x<-(oto.new[[otoname]]%>%as.data.frame())$V1/(dim(img)[2])+1#x
    #  y<-(oto.new[[otoname]]%>%as.data.frame())$V2/(dim(img)[1])+1#y
    jpeg(filename = pathi, width = im_height, height = im_width, units = "px")
    plot(1:2, type='n')
    rasterImage(img,1,1,2,2)
    points(x,y, col="red",cex=cex)
    dev.off()
  }
  message("3. All superimposed pictures are produced!")
  
  stopCluster(cl)
  
  if (length(errors) == 0){
    message("Otolith extraction finished! Any errors?\n--> No errors occurred!")
  } else {
    message("Otolith extraction finished! Any errors?\n--> Yes, see file:")
    print(errors)
  }
  setwd(w.dir)
  ind = names(oto.new)[!names(oto.new)%in%noerror]
  oto.new <- oto.new[!names(oto.new) %in% ind]
  oto <- c(oto,oto.new)
  save(oto,file =  "oto.RData")
  return(oto)
  
  
}
