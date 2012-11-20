package cc.refectorie.proj.bionlp2011

import com.google.common.collect.MapMaker
import com.google.common.base.Function


/**
 * @author sriedel
 */
sealed trait Memo[K, V] {
  def apply(z: K => V): K => V
}

trait Memos {
  def memo[K, V](f: (K => V) => K => V): Memo[K, V] = new Memo[K, V] {
    def apply(z: K => V) = f(z)
  }
}

object MemoUtil extends Memos {

  /**
   * A caching wrapper for a function (K => V), backed by a ConcurrentHashMap from Google Collections.
   */
  def concurrentHashMapMemo[K, V]: Memo[K, V] = {
    memo[K, V] {
      (f: (K => V)) =>
      val map: java.util.concurrent.ConcurrentMap[K, V] = new MapMaker().makeComputingMap(f)
        (k: K) => map.get(k)
    }
  }

  implicit def ScalaFunctionToGoogleFuntion[T, R](f: T => R): Function[T, R] = new Function[T, R] {
    def apply(p1: T) = f(p1)
  }
}