
/**
  * Problem described here: http://projecteuler.net/problem=67
  */
object ProblemSolver {

  type Path = List[Int]

  def gt[T2](tuple: ((Int, T2), (Int, T2))) = if (tuple._1._1 >= tuple._2._1) tuple._1 else tuple._2
  def f: Int => (Int, List[Int])            = elem => (elem, elem :: Nil)
  def ff: ((Int, (Int, Path))) => (Int, Path) =
    elem => (elem._1 + elem._2._1, elem._1 :: elem._2._2)
  def fff: ((Int, ((Int, Path), (Int, Path)))) => (Int, Path) =
    elem => {
      val i = elem._1
      val t = gt(elem._2._1, elem._2._2)
      (i + t._1, i :: t._2)
    }

  /**
    * Find the optimal solution (path with maximum sum)
    * for every number from the *bottom* line of the triangle
    *
    * @param data
    * @return
    */
  def findOptimalPaths(data: List[List[Int]]): List[Path] = data match {
    case Nil  => List[Path]()
    case data => solve(data).map(_._2)
  }

  /**
    * Choose best path that have maximum sum
    * @param paths
    * @return
    */
  def chooseBestPath(paths: List[Path]): Option[Path] = Some(solve(paths).flatten(e => e._2))

  def solve(data: List[Path]): List[(Int, Path)] = data match {
    case Nil => List[(Int, Path)]((0, List()))
    case data =>
      val line0 = data.reverse.head map f
      @scala.annotation.tailrec
      def procc(lines: List[List[Int]], res: List[(Int, Path)]): List[(Int, Path)] = lines match {
        case Nil => res
        case x :: xs =>
          val acc = (x zip (res zip res.tail)) map fff
          procc(xs, acc)
      }
      procc(data.reverse.tail, line0)
  }

}
