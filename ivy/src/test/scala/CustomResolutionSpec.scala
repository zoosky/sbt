package sbt

import org.specs2._
import org.apache.ivy.core
import core.resolve.ResolveOptions

class CustomResolutionSpec extends BaseIvySpecification {
  def is = args(sequential = true) ^ s2"""

  This is a specification to check the custom dependencies resolution

  Calling custom resolution should
    return an update report                                     $e1

  Exclusion rules should
    exclude                                                     $e2
                                                                """

  def commonsIo13 = ModuleID("commons-io", "commons-io", "1.3", Some("compile"))
  def commonsIo13Test = ModuleID("commons-io", "commons-io", "1.3", Some("test->test"))
  def unfilteredUploads080 = ModuleID("net.databinder", "unfiltered-uploads", "0.8.0", Some("compile")) cross CrossVersion.binary
  def unfilteredUploads080Ex = unfilteredUploads080 exclude ("commons-io", "commons-io")

  def customResolution = true

  def e1 = {
    log.setLevel(Level.Debug)
    val m = module(defaultModuleId, Seq(commonsIo13), Some("2.10.3"), UpdateOptions().withCustomResolution(customResolution))
    val report = ivyUpdate(m)
    println(report.allFiles)
    report.configurations.size must_== 3
  }

  def e2 = {
    log.setLevel(Level.Debug)
    val m = module(defaultModuleId, Seq(unfilteredUploads080Ex), Some("2.10.3"), UpdateOptions().withCustomResolution(customResolution))
    val report = ivyUpdate(m)
    val fileNames = report.allFiles map { _.getName }
    println(fileNames)
    fileNames must not contain ("commons-io-1.4.jar")
    // report.configurations.size must_== 3
  }
}
