package sbt

import org.specs2._
import org.apache.ivy.core
import core.resolve.ResolveOptions

class CustomResolutionSpec extends BaseIvySpecification {
  def is = s2"""

  This is a specification to check the custom dependencies resolution

  Calling custom resolution should
    return an update report                                     $e1
                                                                """

  def commonsIo13 = ModuleID("commons-io", "commons-io", "1.3", Some("compile"))
  def commonsIo13Test = ModuleID("commons-io", "commons-io", "1.3", Some("test->test"))

  def customResolution = true

  def e1 = {
    log.setLevel(Level.Debug)
    val m = module(defaultModuleId, Seq(commonsIo13, commonsIo13Test), Some("2.10.2"), UpdateOptions().withCustomResolution(customResolution))
    val report = ivyUpdate(m)
    println(report)
    println(report.configurations.head.modules.head.artifacts)
    report.configurations.size must_== 3
  }
}
