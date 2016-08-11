package org.fusionatlas.enumerators

import net.tqft.toolkit.amazon.S3

object EmptyBucketsApp extends App {
  S3("fusionatlas-extensions").clear
  S3("fusionatlas-fusion-extensions").clear
}