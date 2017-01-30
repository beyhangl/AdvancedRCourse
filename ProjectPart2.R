setClass("room",
         slots=list(ID="numeric",
                    Visit="numeric",
                    Room="character",
                    values = "data.frame"))
setClass("visit",
         slots=list(id="numeric",
                    visit="numeric",
                    room="list"))
setClass("subject",
         slots=list(id="numeric",
                    visit = "list"))

setClass("LongitudinalData",
         slots = list(subjects = "list"))
setGeneric("make_LD", function(x){
  standardGeneric("make_LD")
})
setGeneric("subject", function(x,y){
  standardGeneric("subject")
})
setGeneric("visit", function(x,y){
  standardGeneric("visit")
})
setGeneric("room", function(x,y){
  standardGeneric("room")
})

setMethod("make_LD",
          c(x = "tbl_df"),
          function(x){
            newsubject=list()
            for(i in unique(x$id)){
              newvisitlist=list()
              for(j in unique(x[x$id==i,]$visit)){
                newroom=list()
                for(k in unique(x[x$id==i & x$visit==j,]$room)){
                  newvalues=x[x$id==i & x$visit==j & x$room==k,4:5]
                  newroom[[k]]=new("room",ID=i,Visit=j,Room=k,values=newvalues)
                }
                newvisitlist[[j+1]]=new("visit",id=i,visit=j,room=newroom)
              }
              newsubject[[i]]=new("subject",id=i,visit=newvisitlist)
            }
            new("LongitudinalData",subjects=newsubject)
          })
setMethod(f="subject",
          signature=c(x="LongitudinalData",y="numeric"),
          definition=function(x,y){
            x@subjects[[y]]
          })
setMethod("visit",
          c(x = "subject",y="numeric"),
          function(x,y){
            x@visit[[y+1]]
          })
setMethod("room",
          c(x = "visit",y="character"),
          function(x,y){
            x@room[[y]]
          })

setGeneric("print")
setMethod("print",
          c(x = "LongitudinalData"),
          function(x){
            paste("Longitudinal data with", length(x@subjects)-sum(sapply(x@subjects,is.null)), "subjects")
          })
setMethod("print",
          c(x = "subject"),
          function(x){
            paste("Subject ID: ", x@id)
          })
setMethod("print",
          c(x = "room"),
          function(x){
            cat(paste("ID:", x@ID), paste("Visit:", x@Visit), paste("Room:",x@Room),sep='\n')
          })

setGeneric("summary")
setMethod('summary',
          "subject",
          function(object,...){
            outdata=data.frame(visit=numeric())
            for(i in object@visit){
              outdata[(i@visit+1),1]=i@visit
              for(j in i@room){
                outdata[(i@visit+1),j@Room]=colMeans(j@values[,1])
              }
            }
            outdata
          })
setMethod("summary",
          "room",
          function(object,...){
            summary(object@values[,1])
          })