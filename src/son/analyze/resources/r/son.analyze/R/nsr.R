library(purrr)

#' Deals with Network Service Record (NSR)
#' @name nsd
NULL

Nsd <- R6::R6Class(
  "Nsd",
  public = list(
    nsr = NULL, # a named list corresponding to the json structure
    vnfrs = NULL, # a dict (named list) from string id to a json structure

    ## initialize = function(jsonNsr = "", jsonVnfrs = list()) {
    ##   self$nsr <- rjson::fromJSON(json_str = jsonNsr)
    ##   parseVnfr <- function(jsonVnfr) {
    ##     return(rjson::fromJSON(jsonVnfr))
    ##   }
    ##   self$vnfrs <- lapply(jsonVnfrs, parseVnfr)
    ## },

    initialize = function(yamlObjNsd, yamlObjVnfrs = list()) {
      self$nsr <- yamlObjNsd
      self$vnfrs <- yamlObjVnfrs
    },

    getConnectionPoint = function(id) {
      return(purrr::detect(self$nsr$connection_points, ~ .x$id == id))
    },

    getNetworkFunction = function(id) {
      return(purrr::detect(self$nsr$network_functions, ~ .x$vnf_id == id))
    },

    loadNetworkFunction = function(id) {
      return(self$vnfrs[[id]])
    },

    toIgraph = function() {

      vertices <- local({
        addConnectionPoints <- function(acc, cps) {
          addConnectionPoint <- function(acc, cp) {
            if (acc$prefix == "")
              ref <- cp$id
            else
              ref <- paste0(acc$prefix, ":", cp$id)
            acc$vs[[length(acc$vs) + 1]] <-
              igraph::vertices(ref, prefix = acc$prefix, id = cp$id)
            #acc$ig <- acc$ig + igraph::vertices(paste0(acc$prefix, cp$id))
            return(acc)
          }
          reduce(.x = cps, .f = addConnectionPoint, .init = acc)
        }
        walkNf <- function(acc, nf) {
          vnfd <- self$loadNetworkFunction(nf$vnf_id)
          acc$prefix <- nf$vnf_id
          addConnectionPoints(acc, vnfd$connection_points)
        }
        factorizePrefix <- function(ig) {
          igraph::V(ig)$prefix <- factor(igraph::V(ig)$prefix)
          ig
        }
        list(vs = list(), prefix = "") %>%
          addConnectionPoints(., self$nsr$connection_points) %>%
          reduce(.x = self$nsr$network_functions, .f = walkNf, .init = .) %>%
          reduce(.x = .$vs, .f = `+`, .init = igraph::make_empty_graph()) %>%
          factorizePrefix(.)
      })
      edges <- local({
        target <- self$nsr$forwarding_graphs
        if (length(target) < 1)
          return(c())
        target <- target[[1]]$network_forwarding_paths
        if (length(target) < 1)
          return(c())
        fpId <- target[[1]]$fp_id
        l <- sort_by(target[[1]]$connection_points, ~ .x$position)
        if (length(l) %% 2 != 0)
          stop("The `connection_point_ref` list must be even")
        createEdges <- function(acc, cp) {
          ## print(c("acc=", acc))
          acc$edgs[[length(acc$edgs) + 1]] <-
            igraph::edge(acc$prev$connection_point_ref,
                         cp$connection_point_ref, fpId = fpId)
          acc$prev <- cp
          acc
        }
        #print(c("l=", l[-1]))
        l[-1] %>%
          reduce(.x = ., .f = createEdges,
                 .init = list(prev = l[[1]], edgs = list())) %>%
          .$edgs
      })

      ## networkFPToIgraph <- function(nfp) {
      ##   connectPointToIgraph <- function(acc, cp) {
      ##     target <- self$getConnectionPoint(cp$connection_point_ref)
      ##     #browser()
      ##     if (is.null(target)) {
      ##       keys <- strsplit(cp$connection_point_ref, ":")[[1]]
      ##       if (length(keys) <= 1) {
      ##         extra <- "The NSD contains no `connection_points` with such `id`"
      ##         stop(private$invalidReference(cp$connection_point_ref, exta))
      ##       }
      ##       remoteCP <- self$loadNetworkFunction(keys[[1]])$connection_points
      ##       target <- purrr::detect(remoteCP, ~ .x$id == keys[[2]])
      ##       if (is.null(target)) {
      ##         stop("bis")
      ##       } else {
      ##         return(acc + igraph::vertices(cp$connection_point_ref))
      ##       }
      ##     } else {
      ##       return(acc + igraph::vertices(target$id))
      ##     }
      ##   }
      ##   purrr::reduce(.x = purrr::sort_by(nfp$connection_points, ~ .x$position),
      ##                 .f = connectPointToIgraph,
      ##                 .init = igraph::make_empty_graph())
      ## }
      ## fgToIgraph <- function(fg) {
      ##   return(lapply(fg$network_forwarding_paths, networkFPToIgraph))
      ## }
      ## # return(lapply(self$nsr$forwarding_graphs, fgToIgraph))

      f <- function(acc, x) {
        print(acc)
        acc + x
      }
      reduce(.x = edges, .f = f, .init = vertices) %>%
        list(.) %>%
        list(.)
        #walk(.x = ., .f = ~ print(class(.x))) %>%
        #list(list(.))
    }
  ),

  private = list(
    invalidReference = function(ref, extra,  call = sys.call(-1)) {
      msg <- sprintf("%s refers to nothing or is invalid: %s.", ref, extra)
      structure(
        list(message = msg, call = call, ref = ref),
        class = c("invalidReference", "error", "condition")
      )
    }
  )
)



## library(purrr)
## library(igraph)

## nsdSonataDemo <- local({
##   y <- yaml::yaml.load_file("/var/tmp/son.analyze/tests/fixtures/nsd_sonata-demo.yml")
##   subVnfds <- {
##     l <- c("firewall-vnfd.yml", "iperf-vnfd.yml", "tcpdump-vnfd.yml") %>%
##       map(., ~ paste0("/var/tmp/son.analyze/tests/fixtures/", .x)) %>%
##       map(., ~ yaml::yaml.load_file(.x))
##     names(l) <- c("vnf_firewall", "vnf_iperf", "vnf_tcpdump")
##     l
##   }
##   son.analyze:::Nsd$new(y, subVnfds)
## })

## g <- nsdSonataDemo$toIgraph()[[1]][[1]]
## V(g)$color <- V(g)$prefix
## E(g)$arrow.size <- 0.3
## plot(g)
## adj <- get.adjacency(g, sparse = FALSE)
