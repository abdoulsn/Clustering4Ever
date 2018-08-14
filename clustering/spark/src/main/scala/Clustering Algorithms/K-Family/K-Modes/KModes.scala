package clustering4ever.spark.clustering.kmodes

import scala.collection.{immutable, mutable}
import scala.util.Random
import org.apache.spark.{SparkContext, HashPartitioner}
import org.apache.spark.rdd.RDD
import scala.annotation.meta.param
import scala.reflect.ClassTag
import scala.math.{min, max}
import clustering4ever.math.distances.BinaryDistance
import _root_.clustering4ever.math.distances.binary.Hamming
import _root_.clustering4ever.clustering.ClusteringAlgorithms
import _root_.clustering4ever.util.SumArrays
import _root_.clustering4ever.spark.clustering.accumulators.{CentroidsBinaryAccumulator, CardinalitiesAccumulator}
import _root_.clustering4ever.clustering.datasetstype.DataSetsTypes
import _root_.clustering4ever.scala.clusterizables.BinaryClusterizable
import _root_.org.apache.spark.storage.StorageLevel

/**
 * @author Beck Gaël
 * The famous K-Means using a user-defined dissmilarity measure.
 * @param data : an immutable.Seq with and ID and the vector
 * @param k : number of clusters
 * @param epsilon : minimal threshold under which we consider a centroid has converged
 * @param iterMax : maximal number of iteration
 * @param metric : a defined dissimilarity measure, it can be custom by overriding BinaryDistance distance function
 **/
class KModes[ID: Numeric, Obj <: Serializable](
	@transient val sc: SparkContext,
	data: RDD[BinaryClusterizable[ID, Obj]],
	var k: Int,
	var epsilon: Double,
	var maxIter: Int,
	var metric: BinaryDistance[immutable.Seq[Int]],
	val initializedCenters: mutable.HashMap[Int, immutable.Seq[Int]] = mutable.HashMap.empty[Int, immutable.Seq[Int]],
	var persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY
) extends ClusteringAlgorithms[ID, immutable.Seq[Int]]
{
	val binaryDS = data.map(_.vector).persist(persistanceLVL)

	type CentersMap = mutable.HashMap[Int, immutable.Seq[Int]]

	def obtainNearestModID(v: immutable.Seq[Int], kModesCenters: CentersMap): Int =
	{
		kModesCenters.minBy{ case(clusterID, mode) => metric.d(mode, v) }._1
	}

	def run(): KModesModel =
	{
		val dim = binaryDS.first.size
		val centers = if( initializedCenters.isEmpty ) mutable.HashMap((for( clusterID <- 0 until k ) yield( (clusterID, immutable.Seq.fill(dim)(Random.nextInt(2))) )):_*) else initializedCenters
		val centersCardinality = centers.map{ case (clusterID, _) => (clusterID, 0L) }
		val centersUpdated = centers.clone
		var cpt = 0
		var allModHaveConverged = false
		while( cpt < maxIter && ! allModHaveConverged )
		{
			if( metric.isInstanceOf[Hamming] )
			{
				val info = binaryDS.map( v => (obtainNearestModID(v, centers), (1L, v)) ).reduceByKey{ case ((sum1, v1), (sum2, v2)) => (sum1 + sum2, SumArrays.sumArraysNumerics[Int](v1, v2)) }.map{ case (clusterID, (cardinality, preMode)) => (clusterID, preMode.map( x => if( x * 2 >= cardinality ) 1 else 0 ), cardinality) }.collect

				info.foreach{ case (clusterID, mode, cardinality) =>
				{
					centersUpdated(clusterID) = mode
					centersCardinality(clusterID) = cardinality
				}}

				allModHaveConverged = centers.forall{ case (clusterID, uptMod) => metric.d(centers(clusterID), uptMod) <= epsilon }
				
				centersUpdated.foreach{ case (clusterID, mode) => centers(clusterID) = mode }
			}
			else
			{
				println("Results will have no sense or cost O(n²) for the moment with another distance than Hamming, but we're working on it")
			}
			cpt += 1
		}
		new KModesModel(centers, metric)
	}
}

object KModes
{
	def run[ID: Numeric, Obj <: Serializable](
		@(transient @param) sc: SparkContext,
		data: RDD[BinaryClusterizable[ID, Obj]],
		k: Int,
		epsilon: Double,
		maxIter: Int,
		metric: BinaryDistance[immutable.Seq[Int]],
		initializedCenters: mutable.HashMap[Int, immutable.Seq[Int]] = mutable.HashMap.empty[Int, immutable.Seq[Int]],
		persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY): KModesModel =
	{
		val kmodes = new KModes[ID, Obj](sc, data, k, epsilon, maxIter, metric, initializedCenters, persistanceLVL)
		val kModesModel = kmodes.run()
		kModesModel
	}
}