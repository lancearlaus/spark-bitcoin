package com.spark.bitcoin

import org.apache.spark.rdd.RDD
import org.apache.spark.sql.{SQLContext, Row}
import org.apache.spark.sql.sources.{BaseRelation, PrunedScan}
import org.apache.spark.sql.types.StructType

class BitcoinRelation(location: String)(@transient val sqlContext: SQLContext) extends BaseRelation with PrunedScan {

  override def schema: StructType = ???

  override def buildScan(requiredColumns: Array[String]): RDD[Row] = ???
}
