package org.clustering4ever.preprocessing

import breeze.linalg._
import breeze.linalg.svd.SVD
/**
 *
 */
final case class PCAModel(final val dm: DenseMatrix[Double]) {
  /**
   *
   */
  final def project(point: DenseVector[Double], component: Int): DenseVector[Double] = {
    val eigenVectors = dm.delete((component + 1) until dm.cols , Axis._1)
    eigenVectors * point
  }
  /**
   *
   */
  final def project(matrix: DenseMatrix[Double], component: Int = 2): DenseMatrix[Double] = {
    val eigenVectors = dm.delete((component + 1) until dm.cols, Axis._1)
    matrix * eigenVectors
  }
  /**
   *
   */
  final def project(data: Array[Array[Double]], component: Int): DenseMatrix[Double] = {
    val dm = DenseMatrix(data:_*)
    project(dm, component)
  }
}
/**
 *
 */
object PCA {

  final def fit(dm: DenseMatrix[Double], component: Int = 0): PCAModel = {
    val dim = if(component == 0) dm.cols else component
    val d = zeroMean(dm)
    val SVD(_, _, v) = svd(d.t)
    val eigenv = v(0 until dim, ::)
    val filter = eigenv.t * eigenv
    PCAModel(filter * d)
  }

  final def fit(data: Array[Array[Double]], component: Int): PCAModel = {
    val dm = DenseMatrix(data:_*)
    fit(dm, component)
  }

  private final def mean(v: Vector[Double]) = v.valuesIterator.sum / v.size

  private final def zeroMean(m: DenseMatrix[Double]): DenseMatrix[Double] = {
    val copy = m.copy
    (0 until m.cols).foreach{ c =>
      val col = copy(::, c)
      val colMean = mean(col)
      copy(::, c) -= colMean
    }
    copy
  }

  // val bufferedSource = scala.io.Source.fromFile("C:\\Users\\ATTAOUI\\Documents\\SS_GStream\\resources\\waveform.txt")
  // val lines = (for (line <- bufferedSource.getLines()) yield line).toSeq
  // val data = lines.map(x => x.split(',').map(_.toDouble)).toArray
  // val pca = fit(data).project(data,2)
  // println(pca)

}