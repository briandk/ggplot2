geom_marginaldotplot <- function (mapping = NULL, data = NULL, stat = "bindot", position = "identity",
na.rm = FALSE, binwidth = NULL, binaxis = "x", method="dotdensity", binpositions = "bygroup", stackdir = "up",
stackratio = 1, dotsize = 1, ...) {
  GeomDotplot$new(mapping = mapping, data = data, stat = stat, position = position,
  na.rm = na.rm, binwidth = binwidth, binaxis = binaxis, method = method, binpositions = binpositions,
  stackdir = stackdir, stackratio = stackratio, dotsize = dotsize, ...)
}

GeomDotplot <- proto(Geom, {
  objname <- "marginaldotplot"

  new <- function(., mapping = NULL, data = NULL, stat = NULL, position = NULL, ...){
    # This code is adapted from Layer$new. It's needed to pull out the stat_params
    # and geom_params, then manually add binaxis to both sets of params. Otherwise
    # Layer$new will give binaxis only to the geom.

    stat <- Stat$find(stat)
    match.params <- function(possible, params) {
      if ("..." %in% names(possible)) {
        params
      } else {
        params[match(names(possible), names(params), nomatch = 0)]
      }
    }

    params <- list(...)
    geom_params <- match.params(.$parameters(), params)
    stat_params <- match.params(stat$parameters(), params)
    stat_params <- stat_params[setdiff(names(stat_params), names(geom_params))]
    # Add back binaxis
    stat_params <- c(stat_params, binaxis=params$binaxis)

    do.call("layer", list(mapping = mapping, data = data, stat = stat, geom = ., position = position,
                          geom_params = geom_params, stat_params = stat_params, ...))
  }


  reparameterise <- function(., df, params) {
    df$width <- df$width %||% 
      params$width %||% (resolution(df$x, FALSE) * 0.9)

    # Set up the stacking function and range
    if(params$stackdir == "up") {
      stackdots <- function(a)  a - .5
      stackaxismin <- 0
      stackaxismax <- 1
    } else if (params$stackdir == "down") {
      stackdots <- function(a) -a + .5
      stackaxismin <- -1
      stackaxismax <- 0
    } else if (params$stackdir == "center") {
      stackdots <- function(a)  a - 1 - max(a - 1) / 2
      stackaxismin <- -.5
      stackaxismax <- .5
    } else if (params$stackdir == "centerwhole") {
      stackdots <- function(a)  a - 1 - floor(max(a - 1) / 2)
      stackaxismin <- -.5
      stackaxismax <- .5
    }

    if (params$binaxis == "x") {
      # Fill the bins: at a given x, if count=3, make 3 entries at that x, with
      # coutidx=1,2,3, and set stackpos according to stack function
      df <- ddply(df, .(x, group), function(xx) {
                      if(xx$count == 0) return(NULL)
                      xx[1:xx$count, ] <- xx[1, ]   # replicate the row count times
                      xx$countidx <- 1:(xx$count[1])
                      xx$stackpos <- stackdots(xx$countidx)
                      xx
                    })

      # ymin, ymax, xmin, and xmax define the bounding rectangle for each stack
      # Can't do bounding box per dot, because y position isn't real.
      # After position code is rewritten, each dot should have its own bounding box.
      df$xmin <- df$x - df$binwidth / 2
      df$xmax <- df$x + df$binwidth / 2
      df$ymin <- stackaxismin
      df$ymax <- stackaxismax
      df$y    <- 0

    } else if (params$binaxis == "y") {
      # Fill the bins: at a given y, if count=3, make 3 entries at that y, with
      # coutidx=1,2,3, and set stackpos according to stack function
      df <- ddply(df, .(y, group), function(xx) {
                      if(xx$count == 0) return(NULL)
                      xx[1:xx$count, ] <- xx[1, ]   # replicate the row count times
                      xx$countidx <- 1:(xx$count[1])
                      xx$stackpos <- stackdots(xx$countidx)
                      xx
                    })

      # ymin, ymax, xmin, and xmax define the bounding rectangle for each stack
      # Can't do bounding box per dot, because x position isn't real.
      # xmin and xmax aren't really the x bounds, because of the odd way the grob
      # works. They're just set to the standard x +- width/2 so that dot clusters
      # can be dodged like other geoms.
      # After position code is rewritten, each dot should have its own bounding box.
      df <- ddply(df, .(group), transform,
            ymin = min(y) - binwidth[1] / 2,
            ymax = max(y) + binwidth[1] / 2)

      df$xmin <- df$x + df$width * stackaxismin
      df$xmax <- df$x + df$width * stackaxismax
      # Unlike with y above, don't change x because it will cause problems with dodging
    }
    df
  }
  
  draw <- function(., data, scales, coordinates, na.rm = FALSE, binaxis = "x",
                   stackdir = "up", stackratio = 1, dotsize = 1, ...) {
    data <- remove_missing(data, na.rm, 
      c("x", "y", "size", "shape"), name = "geom_marginaldotplot")
    if (empty(data)) return(zeroGrob())

    if (!is.linear(coordinates)) {
      warning("geom_marginaldotplot does not work properly with non-linear coordinates.")
    }

    tdata <- coord_transform(coordinates, data, scales)

    # Swap axes if using coord_flip
    if ("flip" %in% attr(coordinates, "class"))
      binaxis <- ifelse (binaxis == "x", "y", "x")

    if (binaxis == "x") {
      stackaxis = "y"
      dotdianpc <- dotsize * tdata$binwidth[1] / (max(scales$x.range) - min(scales$x.range))

    } else if (binaxis == "y") {
      stackaxis = "x"
      dotdianpc <- dotsize * tdata$binwidth[1] / (max(scales$y.range) - min(scales$y.range))
    }

    ggname(.$my_name(),
      dotstackGrob(stackaxis = stackaxis, x = tdata$x, y = tdata$y, dotdia = dotdianpc,
                  stackposition = tdata$stackpos, stackratio = stackratio,
                  default.units = "npc",
                  gp = gpar(col = alpha(tdata$colour, tdata$alpha),
                          fill = alpha(tdata$fill, tdata$alpha)))
    )
  }

  guide_geom <- function(.) "marginaldotplot"
  draw_legend <- function(., data, ...) {
    data$shape <- 21

    data <- aesdefaults(data, .$default_aes(), list(...))
    
    with(data,
      pointsGrob(0.5, 0.5, size = unit(.5, "npc"), pch = shape,
        gp = gpar(
          col = alpha(colour, alpha), 
          fill = alpha(fill, alpha))
      )
    )
  }

  icon <- function(.) {
    xpos <- c(1,1,2,3,3,3,4,4,5,5,5,5,6,7,7,7,8,8,9)/10
    ypos <- c(1,2,1,1,2,3,1,2,1,2,3,4,1,1,2,3,1,2,1)/10
    pointsGrob(x = xpos, y = ypos, pch = 19, size = unit(.1, "npc"),
               gp = gpar(col = "black", cex = 0.5), default.units = "npc")
  }
  
  default_stat <- function(.) StatBindot
  required_aes <- c("x", "y")
  default_aes <- function(.) aes(y=..count.., colour="black", fill = "black", alpha = 1)
  
})
