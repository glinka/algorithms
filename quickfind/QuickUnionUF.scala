import Array._
import scala.io.StdIn.{readInt, readLine}

class QuickUnionUF(val n: Int) {

  private var sites: Array[Int] = ofDim[Int](n);
  for (i <- 0 until n) {
    sites(i) = i;
  }
  private var nsites: Int = n;
  def count: Int = nsites;

  def connected(i: Int, j: Int): Boolean = {
    find(i) == find(j);
  }

  def find(i: Int): Int = {
    val iid = sites(i);
    if (iid != i) find(iid);
    else iid;
  }

  def union(i: Int,j: Int): Unit = {
    val iid = find(i);
    val jid = find(j);
    if (iid != jid) {
      sites(iid) = sites(jid);
      nsites -= 1;
    }
  }

}
