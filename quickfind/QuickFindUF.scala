import Array._
import scala.io.StdIn.{readInt, readLine}

class QuickFindUF(val n: Int) {

  private var sites: Array[Int] = ofDim[Int](n);
  for (i <- 0 until n) {
    sites(i) = i;
  }
  private var nsites: Int = n;
  def count: Int = nsites;

  def connected(i: Int, j: Int): Boolean = {
    sites(i) == sites(j);
  }

  def find(i: Int): Int = {
    sites(i)
  }

  def union(i: Int,j: Int): Unit = {
    val iid = sites(i);
    val jid = sites(j);
    if (iid == jid) {
      return;
    }
    for (k  <- 0 until n) {
      if (sites(k) == jid) sites(k) = iid;
    }
    nsites -= 1;
  }

}

// object Demo {
//   def main(args: Array[String]) {
//     val uf = new QuickFindUF(10);
//     uf.union(0,1);
//     uf.union(0,2);
//     uf.union(0,3);
//     uf.union(4,5);
//     uf.union(6,7);
//     uf.union(6,8);
//     println("Have " + uf.count + " components");
//   }
// }


// object QuickFindGen {
//   def main(args: Array[String]) {
//     val n = readInt();
//     val uf = new QuickFindUF(n);
//     var current_string = readLine();
//     while (current_string != null) {
//       val p = current_string(0).toInt();
//       val q = current_string(2).toInt();
//       uf.union(p, q);
//     }
//     println(uf count + " components");
//   }
// }  

