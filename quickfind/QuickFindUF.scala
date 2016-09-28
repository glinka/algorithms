class QuickFind(n: Int) {

  private var sites: Array[Int] = ofDim[Int](n);
  for (i <- 0 to n) {
    sites[i] = i;
  }

  private var nsites: Int = n;

  def count: Int = n;

  def connected(i: Int, j: Int): Boolean = {
    sites[i] == sites[j];
  }

  def find(i: Int): Int = {
    sites[i]
  }

  def union(i: Int,j: Int): Unit = {
    iid = sites[i];
    jid = sites[j];
    for (k  <- 0 to nsites) {
      if (sites[k] == jid) sites[k] = iid;
    }
  }

    public static void main(String[] args) {
        int n = StdIn.readInt();
        QuickFindUF uf = new QuickFindUF(n);
        while (!StdIn.isEmpty()) {
            int p = StdIn.readInt();
            int q = StdIn.readInt();
            if (uf.connected(p, q)) continue;
            uf.union(p, q);
            StdOut.println(p + " " + q);
        }
        StdOut.println(uf.count() + " components");
    }


}

