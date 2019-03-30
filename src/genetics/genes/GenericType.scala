package genetics.genes

import scala.collection.mutable.ListBuffer

/*
  create an object to store the type T instance,
  its fitness value,
  and genes (a list of gene)
 */
class GenericType[T](var fitness:Double, var genes: ListBuffer[Gene], var instance:T)
{

}
