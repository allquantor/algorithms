package com.allquantor

import scala.annotation.tailrec

object DynamicalConnectivity {

  /**
    * Scala implementation of Dynamical Connectivity problem 
    * according to Robert Sedgewick and Kevin Wayne (http://algs4.cs.princeton.edu/15uf/)
    *
    */

  sealed trait DynamicConnectivitySequence {
    /**
      *
      * @param p Element id to union.
      * @param q Element id to union.
      * @return Updated sequence with connected elements.
      */

    def union(p: Int, q: Int): DynamicConnectivitySequence

    /**
      *
      * @param p Element id to check.
      * @param q Element id to check.
      * @return Returns a boolean if the elements are already connected or not
      */

    def connected(p: Int, q: Int): Boolean

    /**
      *
      * @param e Element id to find the root.
      * @return Return the root node id to the given element id.
      */

    def find(e: Int): Int
  }

  abstract class SimpleUnion(elems: IndexedSeq[Int]) extends DynamicConnectivitySequence {

    // The implementation can stay the same for all optimizations. T
    // he only difference is that SimpleUnion and QuickUnion will only have one root -
    // 1-d tree. For QuickUnion the tree size is O(n) and QuickUnionW is O(log2 n). 
    def connected(p: Int, q: Int): Boolean = find(p) == find(q)

    def find(e: Int): Int = getRootOf(e)

    @tailrec
    final def getRootOf(e: Int): Int = {
      val root = elems(e)
      if (root == e) e else getRootOf(root)
    }
  }

  case class QuickFind(elems: IndexedSeq[Int]) extends SimpleUnion(elems) {
    // Simple iterating to change in all children of the flat tree.
    override def union(p: Int, q: Int): DynamicConnectivitySequence = {
      if (!connected(p, q)) {
        val newValue = elems(q)
        val oldValue = elems(p)

        this.copy(elems = elems.zipWithIndex.map {
          case (elem, _) if elem == oldValue => newValue
          case (elem, _) => elem
        })
      } else this
    }
  }

  case class QuickUnion(elems: IndexedSeq[Int]) extends SimpleUnion(elems) {
    // Append to the root to build a tree
    override def union(p: Int, q: Int): DynamicConnectivitySequence = {
      this.copy(elems = elems.updated(getRootOf(p), getRootOf(q)))
    }
  }

  case class QuickUnionW(elems: IndexedSeq[Int], weights: IndexedSeq[Int]) extends SimpleUnion(elems) {
    // Append the smaller tree. 
    override def union(p: Int, q: Int): DynamicConnectivitySequence = {
      if (!connected(p, q)) {

        val rootP = getRootOf(p)
        val rootQ = getRootOf(q)
        val weightP = weights(rootP)
        val weightQ = weights(rootQ)

        if (weightP < weightQ) {
          update(rootP, rootQ)
        } else {
          update(rootQ, rootP)
        }
      } else {
        this
      }
    }

    private def update(eL: Int, eH: Int): DynamicConnectivitySequence = {
      this.copy(elems = elems.updated(eL, eH),
        weights = weights.updated(eH, weights(eL) + weights(eH))
      )
    }
  }
  
}
