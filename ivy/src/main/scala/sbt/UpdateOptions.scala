package sbt

import java.io.File

/**
 * Represents configurable options for update task.
 * While UpdateConfiguration is passed into update at runtime,
 * UpdateOption is intended to be used while setting up the Ivy object.
 *
 * See also UpdateConfiguration in IvyActions.scala.
 */
final class UpdateOptions private[sbt] (
    /** If set to true, check all resolvers for snapshots. */
    val latestSnapshots: Boolean,
    /** If set to true, use consolidated resolution. */
    val consolidatedResolution: Boolean,
    val customResolution: Boolean) {

  def withLatestSnapshots(latestSnapshots: Boolean): UpdateOptions =
    copy(latestSnapshots = latestSnapshots)
  def withConsolidatedResolution(consolidatedResolution: Boolean): UpdateOptions =
    copy(consolidatedResolution = consolidatedResolution)

  def withCustomResolution(customResolution: Boolean): UpdateOptions =
    copy(customResolution = customResolution)

  private[sbt] def copy(
    latestSnapshots: Boolean = this.latestSnapshots,
    consolidatedResolution: Boolean = this.consolidatedResolution,
    customResolution: Boolean = this.customResolution): UpdateOptions =
    new UpdateOptions(latestSnapshots, consolidatedResolution, customResolution)
}

object UpdateOptions {
  def apply(): UpdateOptions =
    new UpdateOptions(
      latestSnapshots = true,
      consolidatedResolution = false,
      customResolution = false)
}
