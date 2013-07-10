package com.meetu.sunspotanalyser

class SunSurfaceService {
  def createSurface(input: List[Int]): List[SunSpot] = {
    val size = input.head
    val heats = input.tail
    require(size * size == heats.size)

    val points = createPoints(size)
    val sunSpots = (0 to points.size - 1).toList map {
      index => SunSpot(points(index).abscissa, points(index).ordinate, heats(index))
    }
    sunSpots
  }

  def createSurfaceAndReturnResult(input: List[Int]): String = {
    val t = input.head
    require(t <= input.tail.tail.size)
    val sunSpots = createSurface(input.tail)
    val heatResults = getHeatResultsOfsurface(sunSpots).sortBy(_.heat).reverse.take(t)
    heatResults.mkString(" ")
  }

  def getHeatsOfsurface(sunSurface: List[SunSpot]): List[Int] = sunSurface map {
    sunSpot =>
      new Result(sunSpot.abscissa, sunSpot.ordinate, getHeatOfSunSpot(sunSurface, sunSpot))
      getHeatOfSunSpot(sunSurface, sunSpot)
  }

  def getHeatResultsOfsurface(sunSurface: List[SunSpot]): List[Result] = sunSurface map {
    sunSpot => new Result(sunSpot.abscissa, sunSpot.ordinate, getHeatOfSunSpot(sunSurface, sunSpot))
  }

  def getHeatOfSunSpot(sunSurface: List[SunSpot], sunSpot: SunSpot) = {
    (neighbours(sunSpot, sunSurface) map { neighbour => neighbour.heat }).sum + sunSpot.heat
  }

  def createPoints(size: Int): List[SunSpot] = for {
    x <- (0 to size - 1).toList
    y <- (0 to size - 1).toList
  } yield SunSpot(x, y, 0)

  def neighbours(sunSpot: SunSpot, sunSurface: List[SunSpot]): List[SunSpot] = {
    val allNeighbours = for {
      deltaX <- List(-1, 0, 1)
      deltaY <- List(-1, 0, 1) if (!(deltaX == 0 && deltaY == 0))
    } yield findSunSpot(sunSurface, sunSpot.abscissa + deltaX, sunSpot.ordinate + deltaY)

    val filteredNbrs = (allNeighbours filterNot (_ == None)) map (_.get)
    filteredNbrs.filter { neighbour => (neighbour.abscissa >= 0 || neighbour.ordinate >= 0) }
  }

  def findSunSpot(sunSurface: List[SunSpot], abscissa: Int, ordinate: Int) = {
    val elements = sunSurface filter {
      sunSpotElement => sunSpotElement.abscissa == abscissa && sunSpotElement.ordinate == ordinate
    }
    elements.isEmpty match {
      case false => Some(elements.head)
      case true => None
    }
  }
}

case class SunSpot(abscissa: Int, ordinate: Int, heat: Int)

class Result(val x: Int, val y: Int, val heat: Int) {
  val comma = ","
  override def toString = "(" + x + comma + y + comma + "score:" + heat + ")"
}