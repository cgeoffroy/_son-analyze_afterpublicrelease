library(purrr, warn.conflicts = FALSE)
#`%>%` <- purrr::`%>%`

context(makeContextTitle("NSD expectations"))

nsdSonataDemo <- local({
  y <- yaml::yaml.load_file("../fixtures/nsd_sonata-demo.yml")
  subVnfds <- {
    l <- c("firewall-vnfd.yml", "iperf-vnfd.yml", "tcpdump-vnfd.yml") %>%
      map(., ~ paste0("../fixtures/", .x)) %>%
      map(., ~ yaml::yaml.load_file(.x))
    names(l) <- c("vnf_firewall", "vnf_iperf", "vnf_tcpdump")
    l
  }
  Nsd$new(y, subVnfds)
})


test_that("Creating a Nsd and reading fields", {
  x <- nsdSonataDemo
  expect_false(is.null(x))
  expect_is(x, "Nsd")
  expect_equal(x$getConnectionPoint("ns:input"),
               structure(list(id = "ns:input", type = "interface"),
                         .Names = c("id", "type")))
  expect_null(x$getConnectionPoint("foobar"))
  expect_false(is.null(x$getNetworkFunction("vnf_iperf")))
  expect_false(is.null(x$vnfrs$vnf_iperf))
  expect_equal(x$loadNetworkFunction("vnf_iperf")$vendor, "eu.sonata-nfv")
})

test_that("Nsd transformation to igraph", {
  x <- nsdSonataDemo
  r <- x$toIgraph()
  print("----------------------------")
  print(r)
  expect_false(is.null(r))
  expect_length(r, 1)
  expect_length(r[[1]], 1)
  ig <- r[[1]][[1]]
  expect_length(igraph::V(ig), 999)
})

rm(nsdSonataDemo)
