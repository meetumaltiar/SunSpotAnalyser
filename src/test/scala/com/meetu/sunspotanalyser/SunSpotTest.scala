package com.meetu.sunspotanalyser

import org.scalatest.FunSuite

class SunSpotTest extends FunSuite {
  val sunSurface = new SunSurfaceService

  test("test neighbours are correct for origin") {
    val size = 3
    val heats = List(4, 2, 3, 2, 2, 1, 3, 2, 1)
    val sunSpots = createSunSurface(size, heats)
    val sunspot = sunSpots.head
    val nbrs = sunSurface.neighbours(sunspot, sunSpots)
    println("Neighbour Of: " + sunspot)
    println(nbrs)
  }

  test("test neighbours are correct for right corner") {
    val size = 3
    val heats = List(4, 2, 3, 2, 2, 1, 3, 2, 1)
    val sunSpots = createSunSurface(size, heats)
    val sunspot = sunSpots(2)
    val nbrs = sunSurface.neighbours(sunspot, sunSpots)
    println("Neighbour Of: " + sunspot)
    println(nbrs)
  }

  test("test neighbours are correct at edges") {
    val size = 3
    val heats = List(4, 2, 3, 2, 2, 1, 3, 2, 1)
    val sunSpots = createSunSurface(size, heats)
    val sunspot = sunSpots(1)
    val nbrs = sunSurface.neighbours(sunspot, sunSpots)
    println("Neighbour Of: " + sunspot)
    println(nbrs)
  }

  def createSunSurface(size: Int, heats: List[Int]): List[SunSpot] = sunSurface.createSurface(size :: heats)

}