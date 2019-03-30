package genetics.genes

/**
  * This class is optional, but recommended as a way to store the genes and fitness in a single object. This
  * can simplify sorting based on fitness. You may add or change anything in this class
  */
class Animal(val genes: List[Gene]) {

  var fitness: Double = 0.0

}
