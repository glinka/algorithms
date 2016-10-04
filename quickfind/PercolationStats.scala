import Array._
import scala.util.Random
import scala.math.pow

class PercolationStats {

  val rng = scala.util.Random;

  def run_trials(n: Int, trials: Int): Unit = {

    var means = ofDim[Double](trials);

    for (i <- 0 until trials) {
      // initialize new union find algorithm for n*n grid with two helper nodes at either end
      var uf = new QuickUnionUF(n*n + 2); // QuickFindUF(n*n + 2);
      var opened: Int = 0;
      // fill grid indicating whether a site is open or closed with zeros (closed) except for two end nodes that are open
      var grid_closed = fill[Boolean](n*n + 2)(true);
      grid_closed(n*n) = false;
      grid_closed(n*n + 1) = false;
      // open sites until the grid percolates
      while (!uf.connected(n*n, n*n + 1)) {
        // choose next site to open (if it is closed)
        val p = rng.nextInt(n*n);
        // if closed, open it and connect with any open neighbors
        if (grid_closed(p)) {
          grid_closed(p) = false;
          opened += 1;

          // check if on left or right edge
          var left_edge = false;
          var right_edge = false;
          if (p % n == 0) {
            left_edge = true;
          }
          else if (p % (n-1) == 0) {
            right_edge = true;
          }

          // check if on top row
          if (p < n) {

            uf.union(n*n, p); // connect to top additional node
            if (!grid_closed(p+n)) uf.union(p, p+n); // always connect to position below if open

            // check if in first position
            if (left_edge) {
              if (!grid_closed(1)) uf.union(0, 1);
            }
            else if (right_edge) {
              if (!grid_closed(p-1)) uf.union(p-1, p);
            }
            else {
              if (!grid_closed(p-1)) uf.union(p-1, p);
              if (!grid_closed(p+1)) uf.union(p, p+1);
            }
          }
          // check if on bottom row
          else if (p >= n*(n-1)) {

            uf.union(n*n+1, p); // connect to bottom additional node
            if (!grid_closed(p-n)) uf.union(p, p-n); // always connect to position above if open

            if (left_edge) {
              if (!grid_closed(p+1)) uf.union(p, p+1);
            }
            else if (right_edge) {
              if (!grid_closed(p-1)) uf.union(p-1, p);
            }
            else {
              if (!grid_closed(p-1)) uf.union(p-1, p);
              if (!grid_closed(p+1)) uf.union(p, p+1);
            }
          }
          // not on top or bottom row
          else {

            if (!grid_closed(p-n)) uf.union(p, p-n); // connect to position above if open
            if (!grid_closed(p+n)) uf.union(p, p+n); // connect to position below if open

            if (left_edge) {
              if (!grid_closed(p+1)) uf.union(p, p+1);
            }
            else if (right_edge) {
              if (!grid_closed(p-1)) uf.union(p-1, p);
            }
            else {
              if (!grid_closed(p-1)) uf.union(p-1, p);
              if (!grid_closed(p+1)) uf.union(p, p+1);
            }
          }
        }
      }
      means(i) = opened/pow(n,2);
    }
    var overall_mean: Double = 0;
    for (mean <- means) {
      overall_mean += mean;
    }
    overall_mean /= trials;

    var stddev: Double = 0;
    for (mean <- means) {
      stddev += pow(mean - overall_mean, 2);
    }
    stddev = pow(stddev/(trials - 1), 0.5);

    println("Average after " + trials + " trials: " + overall_mean);
    println("StdDev after " + trials + " trials: " + stddev);
  }
}

object Demo {
  def main(args: Array[String]) {
    var pstats = new PercolationStats();
    pstats.run_trials(100, 10);
  }
}
