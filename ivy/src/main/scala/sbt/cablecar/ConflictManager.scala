package sbt
package cablecar

import org.apache.ivy.core
import org.apache.ivy.plugins.conflict.{
  ConflictManager => IvyConflictManager,
  NoConflictManager,
  StrictConflictManager,
  LatestConflictManager
}
import org.apache.ivy.plugins.latest.ArtifactInfo
import core.module.descriptor.{ DependencyDescriptor, ModuleDescriptor }

// Wrapper for Ivy's conflict managers
private[sbt] object ConflictManager {
  def resolveConflicts(rootModuleConf: String, org: String, name: String,
    conflicts: Vector[CablecarNode], manager: IvyConflictManager,
    dds: Map[(CablecarNode, CablecarNode), DependencyDescriptor],
    mds: Map[CablecarNode, ModuleDescriptor]): Either[Exception, EvictionEntry] =
    if (conflicts.size < 2) Right(EvictionEntry(manager, conflicts, Vector()))
    else manager match {
      case _: NoConflictManager =>
        Right(EvictionEntry(manager, conflicts, Vector()))
      case _: StrictConflictManager =>
        Left(new RuntimeException("conflict was found in $rootModuleConf:$org:$name " + (conflicts map { _.mrid }).mkString("(", ",", ")")))
      case lcm: LatestConflictManager =>
        useLatest(rootModuleConf, org, name, conflicts, lcm, dds, mds)
      case _ => Left(new RuntimeException(s"Unsupported conflict manager $manager"))
    }
  def useLatest(rootModuleConf: String, org: String, name: String,
    conflicts: Vector[CablecarNode], manager: LatestConflictManager,
    dds: Map[(CablecarNode, CablecarNode), DependencyDescriptor],
    mds: Map[CablecarNode, ModuleDescriptor]): Either[Exception, EvictionEntry] =
    {
      // TODO: dd is specific to dependency relationship, but here we apply it generally
      val allDds = dds.toVector map { case ((p, dep), dd) => (dep, dd) }
      conflicts find { node =>
        allDds filter { case (dep, dd) => dep == node } exists { case (dep, dd) => dd.isForce }
      } match {
        // forced dd is found
        case Some(node) =>
          Right(EvictionEntry(manager, Vector(node), conflicts filterNot { _ == node }))
        case _ =>
          val strategy = manager.getStrategy
          val infos = conflicts map { node =>
            (mds get node) match {
              case Some(md) => CablecarNodeArtifactInfo(node, md.getLastModified)
              case _        => CablecarNodeArtifactInfo(node, 0L)
            }
          }
          Option(strategy.findLatest(infos.toArray, None.orNull)) match {
            case Some(CablecarNodeArtifactInfo(node, _)) =>
              Right(EvictionEntry(manager, Vector(node), conflicts filterNot { _ == node }))
            case _ => Right(EvictionEntry(manager, conflicts, Vector()))
          }
      }
    }
}

private[sbt] case class CablecarNodeArtifactInfo(node: CablecarNode, lastModified: Long) extends ArtifactInfo {
  override def getLastModified: Long = lastModified
  override def getRevision: String = node.mrid.getRevision
}

private[sbt] case class EvictionEntry(manager: IvyConflictManager, remaining: Vector[CablecarNode], evicted: Vector[CablecarNode]) {
  override def toString: String =
    s"EvictionEntry(${manager.toString}," +
      (remaining map { _.mrid }).mkString("Vector(", ",", ")") + "," +
      (evicted map { _.mrid }).mkString("Vector(", ",", ")") +
      ")"
}
