package relation
package compiler

import scala.collection.mutable

import ch.epfl.data.sc.pardis
import pardis.optimization.RecursiveRuleBasedTransformer
import pardis.quasi.TypeParameters._
import pardis.types._
import PardisTypeImplicits._
import pardis.ir._

import relation.deep.RelationDSLOpsPackaged
import relation.shallow._  

class ColumnStoreLowering(override val IR: RelationDSLOpsPackaged, override val schemaAnalysis: SchemaAnalysis) extends RelationLowering(IR, schemaAnalysis) {
  import IR.Predef._

  private var recordsCount = 0

  type LoweredRelation = Rep[Array[Array[String]]] 
  
  def relationScan(scanner: Rep[RelationScanner], schema: Schema, size: Rep[Int], resultSchema: Schema): LoweredRelation = {  
    val schema_size = schema.size
    dsl"""
      val arr = new Array[Array[String]]($schema_size)
      for(k <- 0 until $schema_size) arr(k) = new Array[String]($size)  
      for(i <-0 until $size) {
        for (j <- 0 until $schema_size) arr(j)(i) = $scanner.next_string()
      }
      arr
    """
  }
  
  def relationProject(relation: Rep[Relation], schema: Schema, resultSchema: Schema): LoweredRelation = {
    val arr = getRelationLowered(relation)
    val first_schema = getRelationSchema(relation)
    val schema_size = schema.size
    val schema_columns = schema.columns
    val ind = schema_columns.map(x => first_schema.indexOf(x))
    val indexes = dsl"new Array[Int]($schema_size)"
    for(i <-0 until schema_size) dsl"$indexes($i) = ${ind(i)}"
    
    dsl""" 
      val length = $arr(0).length
      val newArr = new Array[Array[String]]($schema_size)
      for (i <- 0 until $schema_size) {
        newArr(i) = new Array[String](length)
        newArr(i) = $arr($indexes(i)) 
      }
      newArr 
    """
  }
  
  def relationSelect(relation: Rep[Relation], field: String, value: Rep[String], resultSchema: Schema): LoweredRelation = {
    val arr = getRelationLowered(relation)
    val schema = getRelationSchema(relation)
    val schema_size = schema.size
    val index = schema.indexOf(field)

    dsl"""
      var size = 0
      for (j <- 0 until $arr($index).length) {
        if($arr($index)(j) == $value) {
          size = size + 1
        }
      }
      val newArr = new Array[Array[String]]($schema_size)
      for(k <- 0 until $schema_size) newArr(k) = new Array[String](size)
      var i = 0
      for(j <- 0 until $arr($index).length) {
        if($arr($index)(j) == $value) {
          for (k <- 0 until $schema_size) newArr(k)(i) = $arr(k)(j)
          i = i + 1
        }
      }
      newArr
    """
  }
  
  def relationJoin(leftRelation: Rep[Relation], rightRelation: Rep[Relation], leftKey: String, rightKey: String, resultSchema: Schema): LoweredRelation = {
    val arr1 = getRelationLowered(leftRelation)
    val arr2 = getRelationLowered(rightRelation)
    val sch1 = getRelationSchema(leftRelation)
    val sch2 = getRelationSchema(rightRelation)
    val sch1List = sch1.columns
    val sch2List = sch2.columns
    val left_size = sch1List.size
    val right_size = sch2List.size
    val left_index = sch1.indexOf(leftKey)
    val right_index = sch2.indexOf(rightKey)
    val schema_size = left_size+right_size-1

    dsl""" 
      var size = 0 
      val arr1_length = $arr1(0).length
      val arr2_length = $arr2(0).length

      for (i <- 0 until arr1_length; j <- 0 until arr2_length){
        if($arr1($left_index)(i) == $arr2($right_index)(j)) size = size+1
      }

      val newArr = new Array[Array[String]]($schema_size)
      for(k <-0 until $schema_size) newArr(k) = new Array[String](size)  
      
      size = 0
      var right = 0

      for (i <- 0 until arr1_length; j <- 0 until arr2_length){
        if($arr1($left_index)(i) == $arr2($right_index)(j)) {
          for(k <- 0 until $left_size) newArr(k)(size) = $arr1(k)(i)
          right = 0
          for(k <- 0 until $right_size) {
            if(k != $right_index) {
              newArr(right+$left_size)(size) = $arr2(k)(j)
              right = right + 1
            }
          }
          size = size+1
        }
      }
      newArr
    """
  }
  
  def relationPrint(relation: Rep[Relation]): Unit = {
    val arr = getRelationLowered(relation)
    val schema = getRelationSchema(relation)
    val schema_size = schema.size
  
    dsl"""
      var printLine = ""
      for (i <- 0 until $arr(0).length) {
        printLine = ""
        for(j <- 0 until $schema_size-1) {
          printLine = printLine + $arr(j)(i) + "|"
        }
        printLine = printLine + $arr($schema_size-1)(i)
        println(printLine)
      }
    """
  }
}
