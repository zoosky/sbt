package sbt
package cablecar

import org.apache.ivy.core
import core.resolve.{ ResolveData => IvyResolveData }
import core.module.id.{ ArtifactId => IvyArtifactId }
import core.module.descriptor.{ ExcludeRule, DefaultArtifact, Artifact => IvyArtifact }
import org.apache.ivy.plugins.matcher.MatcherHelper

import collection.mutable

private[sbt] final class ExcludeRuleCache {
  import ModuleDescriptorCache._
  val excludeRuleResults: mutable.Map[(String, String, String, String), Boolean] = mutable.Map()

  def isNodeExcluded(rule: ExcludeRule, node: CablecarNode): Boolean =
    {
      val mrid = node.mrid
      val key = (mrid.getOrganisation, mrid.getName, mrid.getRevision, rule.toString)
      excludeRuleResults.getOrElseUpdate(key, {
        isArtifactExcluded(rule, DefaultArtifact.newIvyArtifact(mrid, None.orNull).getId.getArtifactId)
      })
    }
  def isArtifactExcluded(rule: ExcludeRule, artifactId: IvyArtifactId): Boolean =
    MatcherHelper.matches(rule.getMatcher, rule.getId, artifactId)
}
