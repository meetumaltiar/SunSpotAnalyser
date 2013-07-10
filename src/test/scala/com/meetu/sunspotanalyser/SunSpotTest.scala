package com.meetu.sunspotanalyser

import org.scalatest.FunSuite

class SunSpotTest extends FunSuite {
  val sunSurface = new SunSurfaceService

  test("test neighbours are correct for origin") {
    val expected = List(SunSpot(0, 1, 2), SunSpot(1, 0, 2), SunSpot(1, 1, 2))
    val size = 3
    val heats = List(4, 2, 3, 2, 2, 1, 3, 2, 1)
    val sunSpots = createSunSurface(size, heats)
    val sunspot = sunSpots.head
    val nbrs = sunSurface.neighbours(sunspot, sunSpots)
    assert(nbrs === expected)
  }

  test("test neighbours are correct for right corner") {
    val expected = List(SunSpot(0, 1, 2), SunSpot(1, 1, 2), SunSpot(1, 2, 1))
    val size = 3
    val heats = List(4, 2, 3, 2, 2, 1, 3, 2, 1)
    val sunSpots = createSunSurface(size, heats)
    val sunspot = sunSpots(2)
    val nbrs = sunSurface.neighbours(sunspot, sunSpots)
    assert(nbrs === expected)
  }

  test("test neighbours are correct at edges") {
    val expected = List(SunSpot(0, 0, 4), SunSpot(0, 2, 3), SunSpot(1, 0, 2), SunSpot(1, 1, 2), SunSpot(1, 2, 1))
    val size = 3
    val heats = List(4, 2, 3, 2, 2, 1, 3, 2, 1)
    val sunSpots = createSunSurface(size, heats)
    val sunspot = sunSpots(1)
    val nbrs = sunSurface.neighbours(sunspot, sunSpots)
    assert(nbrs === expected)
  }

  test("test that heat count matches") {
    val expected = List(10, 14, 8, 15, 20, 11, 9, 11, 6)
    val size = 3
    val heats = List(4, 2, 3, 2, 2, 1, 3, 2, 1)
    val sunSpots = createSunSurface(size, heats)
    val result = sunSurface.getHeatsOfsurface(sunSpots)
    assert(expected === result)
  }

  test("first test that heat result matches") {
    val expected = "(3,3,score:26)"
    val input = List(1, 5, 5, 3, 1, 2, 0, 4, 1, 1, 3, 2, 2, 3, 2, 4, 3, 0, 2, 3, 3, 2, 1, 0, 2, 4, 3)
    val result = sunSurface.createSurfaceAndReturnResult(input)
    assert(expected === result)
  }

  test("second test that heat result matches") {
    val expected = "(2,1,score:27) (1,1,score:25) (2,2,score:23)"
    val input = List(3, 4, 2, 3, 2, 1, 4, 4, 2, 0, 3, 4, 1, 1, 2, 3, 4, 4)
    val result = sunSurface.createSurfaceAndReturnResult(input)
    assert(expected === result)
  }

  def createSunSurface(size: Int, heats: List[Int]): List[SunSpot] = sunSurface.createSurface(size :: heats)

}