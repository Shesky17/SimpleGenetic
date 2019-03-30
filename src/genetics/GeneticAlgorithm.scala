package genetics

import genetics.genes.{Gene, GenericType}
import genetics.geometry.{Line, _}

import scala.collection.mutable.ListBuffer

object GeneticAlgorithm {

  def geneticAlgorithm[T](fitness: T => Double, map: List[Gene] => T, sampleGList: List[Gene]): T = {
    var countLoop = 0

    //create a list to store all the type T objects
    var aniList = new ListBuffer[GenericType[T]]()

    //create 20 animals with random genes
    for (i <- Range(0, 20)) {
      var tempGeneList = new ListBuffer[Gene]()
      for (i <- sampleGList.indices) {
        tempGeneList += new Gene(Math.random())
      }
      val instance = map(tempGeneList.toList)
      aniList += new GenericType[T](fitness(instance), tempGeneList, instance)
    }

    aniList.sortWith(_.fitness > _.fitness)

    //30000 generations in total
    for (i <- 0 to 35000)
    {
      countLoop += 1 // for testing efficiency

      val best = aniList.head
      val second = aniList(1)

      //mutate the best twice
      for (i <- 0 to 1)
      {
        var newGene: ListBuffer[Gene] = new ListBuffer[Gene]()

        //loop through every gene in the gene list
        for (geneIn <- sampleGList.indices)
        {
          //plus or minus is also randomly decided
          if (Math.random() < 0.5) {
            val _newVal: Double = best.genes.apply(geneIn).geneValue + (Math.random() * 0.01)
            if (_newVal < 1.0) newGene += new Gene(_newVal)
            else newGene += new Gene(1.0)
          }
          else {
            val _newVal: Double = best.genes.apply(geneIn).geneValue - (Math.random() * 0.01)
            if (_newVal < 0.0) newGene += new Gene(0.0)
            else newGene += new Gene(_newVal)
          }
        }
        val instance = map(newGene.toList)
        aniList += new GenericType[T](fitness(instance), newGene, instance)
      }

      var secondList = new ListBuffer[Gene]()

      //mutate second best only once
      for (geneIn <- sampleGList.indices) {
        if (Math.random() < 0.5) {
          val _newVal: Double = second.genes.apply(geneIn).geneValue + (Math.random() * 0.01)
          if (_newVal < 1.0) secondList += new Gene(_newVal)
          else secondList += new Gene(1.0)
        }
        else {
          val _newVal: Double = second.genes.apply(geneIn).geneValue - (Math.random() * 0.01)
          if (_newVal < 0.0) secondList += new Gene(0.0)
          else secondList += new Gene(_newVal)
        }
      }
      val instance = map(secondList.toList)
      aniList += new GenericType[T](fitness(instance), secondList, instance)

      //sort all animals by the fitness value
      aniList.sortWith(_.fitness > _.fitness)

      //a list with the top 4 animals
      var fourList = new ListBuffer[GenericType[T]]
      for (i <- Range(0, 4)) fourList += aniList(i)

      //top 4 animals make children
      for (i <- fourList.indices) {
        for (j <- i + 1 until fourList.length) {
          var newGeneList = new ListBuffer[Gene]()
          for (geIndex <- sampleGList.indices) {
            newGeneList += new Gene((fourList(i).genes(geIndex).geneValue + fourList(j).genes(geIndex).geneValue) / 2)
          }
          val instance = map(newGeneList.toList)
          aniList += new GenericType[T](fitness(instance), newGeneList, instance)
        }
      }

      //add 10 more random
      for (i <- Range(0, 10)) {
        var tempGeneList = new ListBuffer[Gene]()
        for (i <- sampleGList.indices) {
          tempGeneList += new Gene(Math.random())
        }
        val instance = map(tempGeneList.toList)
        aniList += new GenericType[T](fitness(instance), tempGeneList, instance)
      }

      //sort it one more time before starting next generation
      aniList = aniList.sortWith(_.fitness > _.fitness).splitAt(20)._1

      //println(aniList.head.instance) //for testing
      //println(countLoop)
    }

    //return the best one
    aniList.head.instance
  }

  // for testing linear regression
  def linearRegression(ptList: List[Point]): Line = {
    val sampGene = List(new Gene(5), new Gene(6))

    val fitnessMethod = (line: Line) => {
      var sum = 0.0
      for (pt <- ptList) {
        sum += (pt.y - line.evaluate(pt.x)).abs
      }
      mapFit(sum)
    }
    val convertToInf = (x: Double) => {
      Math.tan((x - 0.5) * Math.PI)
    }
    val convertToLine = (geneList: List[Gene]) => {
      new Line(convertToInf(geneList.head.geneValue), convertToInf(geneList(1).geneValue))
    }
    geneticAlgorithm(fitnessMethod, convertToLine, sampGene)
  }

  //map fitness value
  //convert [-inf, inf] to [0, 1]
  //the closer to 1, the better the data
  def mapFit(in: Double): Double = {
    1 - (2 * Math.atan(in)) / Math.PI
  }
}
