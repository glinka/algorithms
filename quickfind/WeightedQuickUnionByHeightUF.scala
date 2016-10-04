import Array._
import scala.io.StdIn.{readInt, readLine}

class WeightedQuickUnionByHeightUF(val n: Int) {

  private var sites: Array[Int] = ofDim[Int](n);
  private var depths: Array[Int] = ofDim[Int](n);
  for (i <- 0 until n) {
    sites(i) = i;
    depths(i) = 0;
  }
  private var nsites: Int = n;
  def count: Int = nsites;

  def connected(i: Int, j: Int): Boolean = {
    find(i) == find(j);
  }

  def find(i: Int): Int = {
    val iid = sites(i);
    if (iid != i) find(iid);
    iid;
  }

  def union(i: Int,j: Int): Unit = {
    val iid = find(i);
    val jid = find(j);
    val depthi = depths(iid);
    val depthj = depths(jid);
    if (iid != jid) {
      // if iid has longer tree, attach jid to it
      if (depthi > depthj) sites(jid) = sites(iid);
      // likewise if jid is longer
      else if (depthj > depthi) sites(iid) = sites(jid);
      // if equal, attach jid to iid and add one to depth of iid
      else {
        sites(jid) = sites(iid);
        depths(iid) += 1;
      }
      nsites -= 1;
    }
  }

}
