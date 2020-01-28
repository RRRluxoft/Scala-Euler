object ProblemSolver {

  type Path = List[Int]
  type Point = (Int, Int)

  /**
    * Find the optimal solution (path with maximum sum)
    * for every number from the *bottom* line of the triangle
    *
    * @param data
    * @return
    */
  def findOptimalPaths(data: List[List[Int]]): List[Path] = {
    val arr = data map (_.toArray) toArray
    val mem = collection.mutable.WeakHashMap[(Int, Int), Stream[Path]]()
    def allPaths(p: Point): Stream[Path] = {
      val (r, c) = p
      val current: Int = arr(r)(c)
      if (r < arr.length - 1) {
        val below: Stream[Path] = {
          if ( (0 until arr(r + 1).length - 1).contains(c)) mem.getOrElseUpdate((r, c), Stream.concat[Path](allPaths(r + 1, c), allPaths(r + 1, c + 1)))
          else mem.getOrElseUpdate((r, c), Stream.concat(allPaths(r + 1, c)))
        }
        below.map(list => current :: list)
      }
      else Stream(current :: Nil)
    }
    allPaths((0, 0)).toList
  }

  /**
    * Choose best path that have maximum sum
    * @param paths
    * @return
    */
  def chooseBestPath(paths: List[Path]): Option[Path] = paths match {
    case Nil   => None
    case paths => Some(paths.maxBy(_.sum)) //paths.find( e => Ordering[Int].gteq(e.sum, e.sum))
  }

  def maxSum(data: List[List[Int]]): Int =
    data.reduceRight(
        (upper: List[Int], lower) =>
          upper zip (lower zip lower.tail) map {
            case (above, (l, r)) => above + Math.max(l, r)
          }
      )
      .head

}
