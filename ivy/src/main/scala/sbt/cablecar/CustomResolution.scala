package sbt
package cablecar

import java.io.File
import java.util.{ Date, Map => JMap }
import org.apache.ivy.Ivy
import org.apache.ivy.core
import org.apache.ivy.plugins.resolver.DependencyResolver
import core.resolve.{ ResolveOptions, ResolveData => IvyResolveData, ResolveEngineSettings, DownloadOptions }
import core.report.{ ResolveReport, ArtifactDownloadReport }
import core.sort.SortOptions
import core.module.id.ModuleRevisionId
import core.module.descriptor.{
  DependencyArtifactDescriptor,
  DefaultModuleDescriptor,
  ModuleDescriptor,
  MDArtifact,
  DependencyDescriptor,
  Configuration => IvyConfig,
  License => IvyLicense,
  Artifact => IvyArtifact
}
import core.IvyContext
import org.apache.ivy.util.filter.{ Filter => IvyFilter }

import collection.mutable

private[sbt] trait CustomResolution { self: Ivy =>
  def mdCache: ModuleDescriptorCache

  // 0.
  def customResolve(md0: ModuleDescriptor, options0: ResolveOptions, log: Logger): UpdateReport = {
    val context = IvyContext.getContext
    try {
      val mrid0 = md0.getResolvedModuleRevisionId
      log.info(s":: resolving dependencies " + mrid0)
      val ivyData = new IvyResolveData(self.getResolveEngine, options0)
      context.setResolveData(ivyData)
      val data0 = ResolveData(ivyData, mdCache, self.getResolveEngine.getSettings, log)
      val (data1, deps) = buildDependencies(md0, options0, data0)
      val data2 = data1.downloadArtifacts(deps, options0.getArtifactFilter, new DownloadOptions())
      data2.buildUpdateReport(deps)
    } finally {
      context.setResolveData(None.orNull)
    }
  }

  // 1.
  /** ResolveEngine#getDependencies */
  def buildDependencies(md0: ModuleDescriptor, options0: ResolveOptions, data0: ResolveData): (ResolveData, Vector[CablecarNode]) = {
    val log = data0.log
    val rootConfigurations = options0.getConfs(md0).toVector
    val missingConfs: Vector[String] =
      rootConfigurations filter { conf => Option(md0.getConfiguration(conf)).isEmpty }
    if (!missingConfs.isEmpty) {
      throw new IllegalArgumentException(
        s"requested ${plural("configuration", missingConfs.size)} not found in ${md0.getModuleRevisionId}:${missingConfs.mkString(", ")}"
      )
    }
    var data1 = data0.copy(rootConfigurations = rootConfigurations)
    val reportDate = new Date()
    val (rootNode, d) = data1.mkCablecarNode(md0)
    data1 = d
    rootConfigurations foreach { conf =>
      log.debug(s":: resolving dependencies for configuration '$conf' (${md0.getModuleRevisionId})")
      val root = CablecarVisitNode(rootNode, None, conf, None)
      data1 = data1.setRequestedConf(root, conf)
      data1 = data1.addConfToFetch(rootNode, conf)
      data1 = data1.fetchDependencies(root, conf, false)
      data1 = data1.clearAllConfsToFetch()
    }
    (data1, data1.sortDependencies.toVector)
  }

  def plural(singular: String, size: Int): String =
    if (size == 1) singular
    else singular + "s"
}

private[sbt] case class ResolveData(
    ivyData: IvyResolveData,
    mdCache: ModuleDescriptorCache,
    settings: ResolveEngineSettings,
    log: Logger,
    rootConfigurations: Vector[String] = Vector(),
    currentVisitNode: Option[CablecarVisitNode] = None,
    nodes: Map[ModuleRevisionId, CablecarNode] = Map(),
    confsToFetches: Map[CablecarNode, Set[String]] = Map(),
    fetchedSet: Set[String] = Set(),
    requestedConfs: Map[CablecarVisitNode, String] = Map(),
    loadedRootModuleConfs: Map[CablecarNode, Set[String]] = Map(),
    dds: Map[(CablecarNode, CablecarNode), DependencyDescriptor] = Map(),
    mds: Map[CablecarNode, ModuleDescriptor] = Map(),
    resolvers: Map[CablecarNode, DependencyResolver] = Map(),
    artifactResolvers: Map[CablecarNode, DependencyResolver] = Map(),
    problems: Map[CablecarNode, Throwable] = Map(),
    usages: Map[CablecarNode, CablecarNodeUsage] = Map(),
    artifacts: Map[CablecarNode, Seq[IvyArtifact]] = Map(),
    artifactDownloadReports: Map[CablecarNode, Seq[ArtifactDownloadReport]] = Map()) {
  val FallBackConfPattern = """(.+)\((.*)\)""".r

  def mkCablecarNode(md: ModuleDescriptor): (CablecarNode, ResolveData) =
    {
      val node = CablecarNode(md.getModuleRevisionId, None)
      (node, copy(mds = mds.updated(node, md)).addNode(node))
    }
  def mkCablecarNode(parent: CablecarNode, dd: DependencyDescriptor): (CablecarNode, ResolveData) =
    {
      val node = CablecarNode(dd.getDependencyRevisionId, Some(parent.root))
      (node, setDependencyDescriptor(node, parent, dd).addNode(node))
    }
  // 1.
  def fetchDependencies(node0: CablecarVisitNode, conf: String, shouldBePublic: Boolean): ResolveData =
    {
      val start = System.currentTimeMillis
      node0.parent match {
        case Some(p) => log.debug(s"== resolving dependencies ${p.mrid} -> ${node0.mrid} [${node0.parentConf.get} -> $conf]")
        case _       => log.debug(s"== resolving dependencies for ${node0.mrid} [$conf]")
      }
      val parentVisitNode = currentVisitNode
      var data = this
      data = copy(currentVisitNode = Some(node0))
      // val dd = node.dependencyDescriptor // ask data?
      // dd map { resolveConflict }
      val (loaded, vOpt, data2) = data.loadCurrentVisitNode(conf, shouldBePublic)
      // log.debug(s"\tafter loading current visit node, usages: ${data2.usages.values.toList}")
      data = data2
      vOpt map { v =>
        if (loaded) {
          // TODO: resolveConflict
          // TODO: check for eviction
          val confs = data.realConfs(v.node, conf)
          // log.debug(s"\tdependencies: ${data.dependencies}")
          confs foreach { c =>
            data = data.doFetchDependencies(v, c)
          }
        } else if (!data.hasProblem(v.node)) {
          // log.warn(s"++ no problem with ${v.node.mrid}")
          // TODO: check for evction
          val confs = data.realConfs(v.node, conf)
          confs foreach { c =>
            data = data.doFetchDependencies(v, c)
          }
        } else {
          // log.warn(s"++ found problem with ${v.node.mrid}")
        }
      }

      data.copy(currentVisitNode = parentVisitNode)
    }
  /** ResolveEngine#doFetchDependencies */
  def doFetchDependencies(v: CablecarVisitNode, conf: String): ResolveData =
    nodeConfiguration(v.node, conf) map { c =>
      var data = this
      val requestedConfSet =
        requestedConf(v) match {
          case None =>
            data = data.setRequestedConf(v, conf)
            true
          case _ => false
        }
      val extendedConfs = c.getExtends.toArray.toVector
      data = data.addConfsToFetch(v.node, extendedConfs)
      extendedConfs foreach { c =>
        data = data.fetchDependencies(v, c, false)
      }
      if (!data.isDependenciesFetched(v.node, conf)) {
        val (deps, data2) = data.visitNodeDependencies(v, conf)
        data = data2
        deps foreach { dep =>
          // TODO use real node
          val depConfs = data.visitNodeRequiredConfigurations(dep, v, conf)
          depConfs foreach { depConf =>
            data = data.fetchDependencies(dep, depConf, true)
          }
          // TODO check evicted
          val confsToFetch = data.nodeConfsToFetch(v.node)
          confsToFetch foreach { depConf =>
            data = data.fetchDependencies(dep, depConf, false)
          }
        }
        data = data.markDependenciesFetched(v.node, conf)
      }
      if (requestedConfSet) {
        data = data.clearRequestedConf(v)
      }
      data
    } getOrElse {
      this
    }
  def visitNodeDependencies(v: CablecarVisitNode, conf: String): (Vector[CablecarVisitNode], ResolveData) =
    {
      val (deps, data) = nodeDependencies(v.node, v.rootModuleConf, conf, requestedConf(v))
      (deps map { node => CablecarVisitNode(node, Some(v), v.rootModuleConf, Some(conf)) }, data)
    }
  /** IvyNode#getDependencies */
  def nodeDependencies(node: CablecarNode, rootModuleConf: String, conf: String, requestedConf: Option[String]): (Vector[CablecarNode], ResolveData) =
    moduleDescriptor(node) match {
      case Some(md) =>
        var data = this
        val dds = md.getDependencies.toVector
        val deps = dds flatMap { dd =>
          val dependencyConfigurations = dd.getDependencyConfigurations(conf, requestedConf.orNull).toVector
          if (dependencyConfigurations.isEmpty) Vector()
          else {
            val requestedDependencyRevisionId = dd.getDependencyRevisionId
            // TODO
            // if (isDependencyModuleExcluded(dd, rootModuleConf, requestedDependencyRevisionId, conf)) {
            //     // the whole module is excluded, it is considered as not being part of dependencies
            //     // at all
            //     Message.verbose("excluding " + dd + " in " + conf);
            //     continue;
            // }
            // log.warn(s"++ nodeDependencies: ${dd}")
            val (depNode, d) = data.getNode(requestedDependencyRevisionId) match {
              case Some(x) => (x, data.setDependencyDescriptor(x, node, dd))
              case None    => data.mkCablecarNode(node, dd)
            }
            data = d.addUsage(depNode, rootModuleConf, dd, conf)
            val rdepsConfs = data.resolveSpecialConfigurations(depNode, dependencyConfigurations)
            data = data.addConfsToFetch(depNode, rdepsConfs)
            // data = data.addConfigurationUsage(depNode, rootModuleConf, rdepsConfs.toSet)
            data = data.setRequiredConfs(depNode, node, conf, rdepsConfs.toSet)
            // log.debug(s"\t${data.usages}")

            Vector(depNode)
          }
        }
        // println(dds.toList)
        (deps, data)
      case _ =>
        throw new IllegalStateException(s"impossible to get dependencies for ${node.mrid} when data has not been loaded")
    }
  def loadCurrentVisitNode(conf: String, shouldBePublic: Boolean): (Boolean, Option[CablecarVisitNode], ResolveData) =
    currentVisitNode map { v =>
      val (loaded, data) = loadData(
        v.node, v.rootModuleConf, v.parentNode, v.parentConf, conf, shouldBePublic)
      if (loaded) {
        // useRealNode()        
        // if the loaded revision is different from original one
        // we now register this node on the new resolved id
        // this includes two cases:
        // - the id refers to a dynamic revision, which has been resolved by loadData
        // - the loaded module descriptor has extra attributes in his info tag which are not 
        //   used when declaring the dependency
        // if (data.getNode(node.getResolvedId()) == null 
        //         || !data.getNode(node.getResolvedId()).getId().equals(node.getResolvedId())) {
        //     data.register(node.getResolvedId(), this);
        // }
      }
      (loaded, Some(v), data)
    } getOrElse (false, None, this)

  // port of IvyNode#loadData
  def loadData(node: CablecarNode, rootModuleConf: String, parentOpt: Option[CablecarNode], parentConfOpt: Option[String], conf: String,
    shouldBePublic: Boolean): (Boolean, ResolveData) =
    {
      log.debug(s"loadData of ${node.mrid} of rootConf=$rootModuleConf")
      val (loaded, data1) = this match {
        case data if data hasProblem node =>
          log.debug(s"Node has problem. Skip loading")
          (false, data)
        case data if !hasConfigurationsToLoad(node) && isRootModuleConfLoaded(node, rootModuleConf) =>
          log.debug(s"$rootModuleConf is loaded and no conf to load. Skip loading")
          (false, data)
        case data0 =>
          val data = data0.markRootModuleConfLoaded(node, rootModuleConf)
          (data.moduleDescriptor(node), parentOpt, parentConfOpt) match {
            case (Some(_), _, _) =>
              // data is already loaded, so we are cool
              (true, data)
            case (None, Some(parent), Some(parentConf)) =>
              Option(settings.getResolver(node.mrid)) match {
                case None => (false, data.nodeError(node, runtimeError(s"no resolver found for ${node.mrid}: check your configuration")))
                case Some(resolver) =>
                  log.debug(s"\tusing $resolver to resolve ${node.mrid}")
                  // line 162 IvyNode.java
                  data.dependencyDescriptor(node, parent) match {
                    case None     => (false, data.nodeError(node, runtimeError(s"no dependency descriptor found for ${node.mrid} from ${parent.mrid}")))
                    case Some(dd) => data.doLoadData(node, dd, resolver)
                  }
              }
            case (None, None, _) => (false, data.nodeError(node, runtimeError(s"Node parent is missing")))
            case (None, _, None) => (false, data.nodeError(node, runtimeError(s"Node parent conf is missing")))
          }
      }
      val data2 = data1.handleConfigurations(node, loaded, rootModuleConf, parentOpt, parentConfOpt, conf, shouldBePublic)
      (loaded, data2)
    }
  def runtimeError(msg: String): RuntimeException = new RuntimeException(msg)
  def doLoadData(node: CablecarNode, dd: DependencyDescriptor, resolver: DependencyResolver): (Boolean, ResolveData) =
    {
      val start = System.currentTimeMillis
      import ModuleDescriptorCache.ModuleDescriptorCacheEntry
      mdCache.loadModuleDescriptor(ivyData, resolver, dd) match {
        case Some(ModuleDescriptorCacheEntry(md, rvr, arvr)) =>
          log.debug(s"\tfound ${node.mrid} in ${resolver.getName}")
          val data2 = copy(mds = mds.updated(node, md),
            resolvers = resolvers.updated(node, rvr),
            artifactResolvers = artifactResolvers.updated(node, arvr))
          (true, data2)
        case None =>
          (false, nodeWarn(node, runtimeError(s"\tmodule not found: ${node.mrid}")))
      }

      /*
      val requestedRevisionId = dd.getDependencyRevisionId
      val moduleOpt = Option(resolver.getDependency(dd, ivyData))
      moduleOpt match {
        case Some(module) =>
          val md = module.getDescriptor
          val resolver = module.getResolver
          val artifactResolver = module.getArtifactResolver
          resolver.getRepositoryCacheManager.saveResolvers(md, resolver.getName, artifactResolver.getName)
          log.debug(s"\tfound ${module.getId} in ${resolver.getName}")

          val data2 = copy(mds = mds.updated(node, md),
            resolvers = resolvers.updated(node, resolver),
            artifactResolvers = artifactResolvers.updated(node, artifactResolver))
          (true, data2)
        case None =>
          (false, nodeWarn(node, runtimeError(s"\tmodule not found: ${node.mrid}")))
      }
      */
    }
  def handleConfigurations(node: CablecarNode, loaded: Boolean, rootModuleConf: String, parentOpt: Option[CablecarNode], parentConfOpt: Option[String], conf: String,
    shouldBePublic: Boolean): ResolveData =
    moduleDescriptor(node) map { md =>
      val confs = realConfs(node, conf)
      var data = addConfigurationUsage(node, rootModuleConf, confs.toSet)
      data
    } getOrElse this

  def visitNodeRequiredConfigurations(v: CablecarVisitNode, parent: CablecarVisitNode, inConfig: String): Set[String] =
    nodeRequiredConfigurations(v.node, parent.node, inConfig)
  def nodeRequiredConfigurations(node: CablecarNode, parent: CablecarNode, inConfig: String): Set[String] =
    {
      val usage = usages.getOrElse(node, CablecarNodeUsage())
      usage.requiredConfs.getOrElse(parent -> inConfig, Set())
    }
  def sortDependencies: Seq[CablecarNode] =
    {
      val xs = nodes.values.toVector
      val nulls = xs filter { x => moduleDescriptor(x).isEmpty }
      val reverseMap: mutable.Map[ModuleDescriptor, Vector[CablecarNode]] = mutable.Map()
      xs foreach { node =>
        moduleDescriptor(node) map { md =>
          val nodes0 = reverseMap.getOrElseUpdate(md, Vector())
          reverseMap(md) = nodes0 :+ node
        }
      }
      import collection.JavaConversions._
      val mdList = ivyData.getEngine.getSortEngine.sortModuleDescriptors(reverseMap.keys.toList, SortOptions.SILENT).toList
      (mdList flatMap { case md: ModuleDescriptor => reverseMap(md) }) ++ nulls
    }
  def downloadArtifacts(deps: Vector[CablecarNode], artifactFilter: IvyFilter, downloadOptions: DownloadOptions): ResolveData =
    {
      var data = this
      deps foreach { node =>
        data.problems.get(node) match {
          case Some(_) => // do nothing
          case None =>
            data.artifactResolvers.get(node) map { resolver =>
              val arts = data.selectedArtifacts(node, artifactFilter)
              // System.err.println(s"arts: $arts")
              val dReport = resolver.download(arts.toArray, downloadOptions)
              val adrs = dReport.getArtifactsReports.toVector
              data = data.copy(artifacts = data.artifacts.updated(node, arts),
                artifactDownloadReports = data.artifactDownloadReports.updated(node, adrs))
            }
        }
      }
      data
    }
  // TODO filter out evicted configurations
  def selectedArtifacts(node: CablecarNode, filter: IvyFilter): Vector[IvyArtifact] =
    (nodeRootModuleConfs(node).toVector.sorted flatMap { conf => nodeArtifacts(node, conf) }).distinct
  /** IvyNode#getArtifacts */
  def nodeArtifacts(node: CablecarNode, rootModuleConf: String): Vector[IvyArtifact] =
    nodeConfigurations(node, rootModuleConf).toVector.sorted match {
      case Vector() => Vector()
      case confs0 =>
        moduleDescriptor(node) map { md =>
          val confs = confs0 flatMap { conf => realConfs(node, conf) }
          val ownArtifacts = nodeDependencyArtifacts(node, rootModuleConf, md)
          if (md.isDefault && !ownArtifacts.isEmpty) {
            ownArtifacts
            // TODO merged usage
          } else {
            // TODO fix this
            val artifacts: mutable.Set[IvyArtifact] = mutable.Set()
            artifacts ++= (confs flatMap { conf =>
              val arts = md.getArtifacts(conf)
              arts
            })
            artifacts ++= ownArtifacts
            artifacts.toVector
          }
        } getOrElse { sys.error(s"impossible to get artifacts when data has not been loaded. CablecarNode = $node") }
    }
  def nodeDependencyArtifacts(node: CablecarNode, rootModuleConf: String, md: ModuleDescriptor): Vector[IvyArtifact] =
    usages.get(node) map { usage =>
      usage.dependers(rootModuleConf).toVector flatMap { depender =>
        depender.dd.getDependencyArtifacts(depender.parentConf) map { dad =>
          new MDArtifact(md, dad.getName, dad.getType, dad.getExt, dad.getUrl, dad.getQualifiedExtraAttributes)
        }
      }
    } getOrElse Vector()

  def buildUpdateReport(deps: Vector[CablecarNode]): UpdateReport =
    {
      new UpdateReport(new File(""), rootConfigurations map { conf => buildConfigurationReport(conf, deps) },
        new UpdateStats(0, 0, 0, false), Map())
    }
  def buildConfigurationReport(rootModuleConf: String, deps0: Vector[CablecarNode]): ConfigurationReport =
    {
      val deps = deps0 filter { isNodeInRootModuleConf(_, rootModuleConf) }
      val moduleReports = deps map { buildModuleReport(_, rootModuleConf) }
      new ConfigurationReport(rootModuleConf, moduleReports, Nil, Nil)
    }
  def buildModuleReport(node: CablecarNode, rootModuleConf: String): ModuleReport =
    {
      def toExtraAttributes(ea: JMap[_, _]): Map[String, String] =
        Map(ea.entrySet.toArray collect {
          case entry: JMap.Entry[_, _] => (entry.getKey.toString, entry.getValue.toString)
        }: _*)

      val mrid = node.mrid
      val moduleId = IvyRetrieve.toModuleID(mrid)
      val branch = Option(mrid.getBranch)
      val mdOpt = moduleDescriptor(node)
      val (status, publicationDate, homepage) = mdOpt match {
        case Some(md) =>
          (Option(md.getStatus),
            Option(md.getPublicationDate),
            Option(md.getHomePage))
        case None => (None, None, None)
      }
      val resolver = resolvers.get(node) map { _.getName }
      val artifactResolver = artifactResolvers.get(node) map { _.getName }
      val (evicted, evictedData, evictedReason) = false match {
        case true =>
          // val ed = dep.getEvictedData(confReport.getConfiguration)
          // (true,
          //   Some(Option(ed.getConflictManager) map { _.toString } getOrElse { "transitive" }),
          //   Option(ed.getDetail))
          (false, None, None)
        case _ => (false, None, None)
      }
      val problem = problems.get(node) flatMap { x => Option(x.getMessage) }
      val extraAttributes: Map[String, String] = mdOpt map { md => toExtraAttributes(md.getExtraAttributes) } getOrElse Map()
      val isDefault = mdOpt map { _.isDefault }
      val configurations = nodeConfigurations(node, rootModuleConf).toSeq.sorted
      val licenses: Seq[(String, Option[String])] = mdOpt match {
        case Some(md) => md.getLicenses.toArray.toVector collect {
          case lic: IvyLicense =>
            (lic.getName, Option(lic.getUrl))
        }
        case _ => Nil
      }
      val callers = Nil // TODO
      val missing = new mutable.ListBuffer[Artifact]
      val resolved = new mutable.ListBuffer[(Artifact, File)]
      for { r <- artifactDownloadReports.getOrElse(node, Vector()) } {
        val fileOpt = Option(r.getLocalFile)
        val art = IvyRetrieve.toArtifact(r.getArtifact)
        fileOpt match {
          case Some(file) => resolved += ((art, file))
          case None       => missing += art
        }
      }
      new ModuleReport(moduleId, resolved.toVector, missing.toVector, status, publicationDate, resolver, artifactResolver,
        evicted, evictedData, evictedReason, problem, homepage, extraAttributes, isDefault, branch,
        configurations, licenses, callers)
    }

  // getter/setter
  // def addDependencyNode(node: CablecarNode): ResolveData =
  //   if (node.isRoot) {
  //     log.debug(s"\t${node.mrid} is a root node, so not adding")
  //     this
  //   } else copy(dependencies = node :: dependencies)
  def addNode(node: CablecarNode): ResolveData =
    copy(nodes = nodes.updated(node.mrid, node))
  def getNode(mrid: ModuleRevisionId): Option[CablecarNode] =
    nodes.get(mrid)
  def moduleDescriptor(node: CablecarNode): Option[ModuleDescriptor] =
    mds.get(node)
  def nodeConfsToFetch(node: CablecarNode): Set[String] =
    confsToFetches.get(node) getOrElse Set()
  def hasConfigurationsToLoad(node: CablecarNode): Boolean =
    nodeConfsToFetch(node).isEmpty
  def clearAllConfsToFetch(): ResolveData =
    copy(confsToFetches = Map())
  def addConfToFetch(node: CablecarNode, conf: String): ResolveData =
    {
      val ctf0 = confsToFetches.get(node) getOrElse Set()
      val ctf = ctf0 + conf
      copy(confsToFetches = confsToFetches.updated(node, ctf))
    }
  def addConfsToFetch(node: CablecarNode, confs: Seq[String]): ResolveData =
    {
      val ctf0 = confsToFetches.get(node) getOrElse Set()
      val ctf = ctf0 ++ confs.toSet
      copy(confsToFetches = confsToFetches.updated(node, ctf))
    }
  def setRequestedConf(v: CablecarVisitNode, conf: String): ResolveData =
    copy(requestedConfs = requestedConfs.updated(v, conf))
  def clearRequestedConf(v: CablecarVisitNode): ResolveData =
    copy(requestedConfs = requestedConfs - v)
  def requestedConf(v: CablecarVisitNode): Option[String] =
    requestedConfs.get(v)
  def resolveSpecialConfigurations(node: CablecarNode, dependencyConfigurations: Vector[String]): Vector[String] =
    dependencyConfigurations match {
      case Vector("*") =>
        moduleDescriptor(node) match {
          case Some(md) =>
            md.getPublicConfigurationsNames.toVector
          case _ => Vector("*")
        }
      case Vector(x) if x startsWith "*" =>
        moduleDescriptor(node) match {
          case Some(md) =>
            val exclusions = x.substring(2).split("""\!""").toVector
            md.getPublicConfigurationsNames.toVector diff exclusions
          case _ => Vector(x)
        }
      case xs => xs
    }
  def nodeWarn(node: CablecarNode, problem: Throwable): ResolveData =
    {
      log.warn(problem.getMessage)
      copy(problems = problems.updated(node, problem))
    }
  def nodeError(node: CablecarNode, problem: Throwable): ResolveData =
    {
      log.error(problem.getMessage)
      copy(problems = problems.updated(node, problem))
    }
  def nodeConfigurations(node: CablecarNode, rootModuleConf: String): Set[String] =
    {
      // TODO handle merged usage
      usages.get(node) flatMap { _.configurations.get(rootModuleConf) } getOrElse Set()
    }
  def isNodeInRootModuleConf(node: CablecarNode, rootModuleConf: String): Boolean =
    usages.get(node) map { usage => usage.configurations contains rootModuleConf } getOrElse false
  def nodeRootModuleConfs(node: CablecarNode): Set[String] =
    usages.get(node) map { usage => usage.configurations.keys.toSet } getOrElse Set()
  def addUsage(node: CablecarNode, rootModuleConf: String, dd: DependencyDescriptor, parentConf: String): ResolveData =
    {
      val usage0 = usages.getOrElse(node, CablecarNodeUsage())
      val ds0 = usage0.dependers.getOrElse(rootModuleConf, Set())
      val usage = usage0.copy(dependers = usage0.dependers.updated(rootModuleConf, ds0 + CablecarNodeUsage.Depender(dd, parentConf)))
      copy(usages = usages.updated(node, usage))
    }
  def setRequiredConfs(node: CablecarNode, parent: CablecarNode, parentConf: String, confs: Set[String]): ResolveData =
    {
      val usage0 = usages.getOrElse(node, CablecarNodeUsage())
      val usage = usage0.copy(requiredConfs = usage0.requiredConfs.updated(parent -> parentConf, confs))
      copy(usages = usages.updated(node, usage))
    }
  def addConfigurationUsage(node: CablecarNode, rootModuleConf: String, confs: Set[String]): ResolveData =
    {
      val usage0 = usages.getOrElse(node, CablecarNodeUsage())
      val cs0 = usage0.configurations.getOrElse(rootModuleConf, Set())
      val usage = usage0.copy(configurations = usage0.configurations.updated(rootModuleConf, cs0 ++ confs))
      copy(usages = usages.updated(node, usage))
    }
  def setDependencyDescriptor(node: CablecarNode, parent: CablecarNode, dd: DependencyDescriptor): ResolveData =
    copy(dds = dds.updated(parent -> node, dd))
  /**
   * returns dependency descriptor of the given `node` from `parent`'s perspective
   * wrapped in Some, if any; otherwise, None.
   */
  def dependencyDescriptor(node: CablecarNode, parent: CablecarNode): Option[DependencyDescriptor] =
    dds.get(parent -> node)
  def nodeConfiguration(node: CablecarNode, conf0: String): Option[IvyConfig] =
    moduleDescriptor(node) map { md =>
      val df = defaultConf(conf0)
      val conf = mainConf(conf0)
      Option(md.getConfiguration(conf.orNull)) orElse Option(md.getConfiguration(df))
    } getOrElse {
      throw new IllegalStateException(s"impossible to get configuration '$conf0' when data has not been loaded for ${node.mrid}")
    }
  def realConfs(node: CablecarNode, conf0: String): Vector[String] =
    moduleDescriptor(node) map { md =>
      val df = defaultConf(conf0) match {
        case "" => None
        case x  => Some(x)
      }
      val mc = mainConf(conf0)
      val confOpt: Option[String] = Option(md.getConfiguration(mc.orNull)) match {
        case None => df
        case Some(x) if x.getVisibility == IvyConfig.Visibility.PRIVATE => df
        case Some(x) => mc
      }
      confOpt match {
        case None                              => Vector()
        case Some(conf) if conf startsWith "*" => resolveSpecialConfigurations(node, Vector(conf))
        case Some(conf) if conf contains ","   => conf.split(",").toVector map { _.trim }
        case Some(conf)                        => Vector(conf)
      }
    } getOrElse Vector(conf0)
  def defaultConf(conf: String): String =
    conf match {
      case FallBackConfPattern(_, c) => c
      case _                         => conf
    }
  def mainConf(conf: String): Option[String] =
    conf match {
      case FallBackConfPattern(main, _) => Some(main)
      case _                            => None
    }
  def markRootModuleConfLoaded(node: CablecarNode, rootModuleConf: String): ResolveData =
    {
      val lrmc = loadedRootModuleConfs.get(node) getOrElse Set()
      copy(loadedRootModuleConfs = loadedRootModuleConfs.updated(node, lrmc + rootModuleConf))
    }
  def isRootModuleConfLoaded(node: CablecarNode, rootModuleConf: String): Boolean =
    (loadedRootModuleConfs.get(node) getOrElse Set())(rootModuleConf)
  def hasProblem(node: CablecarNode): Boolean = problems.contains(node)
  def markDependenciesFetched(node: CablecarNode, conf: String): ResolveData =
    copy(fetchedSet = fetchedSet + s"${node.mrid.toString}%$conf")
  def isDependenciesFetched(node: CablecarNode, conf: String): Boolean =
    fetchedSet contains s"${node.mrid.toString}%$conf"
}

private[sbt] case class CablecarVisitNode(node: CablecarNode, parent: Option[CablecarVisitNode], rootModuleConf: String, parentConf: Option[String]) {
  def mrid: ModuleRevisionId = node.mrid
  def parentNode: Option[CablecarNode] = parent map { _.node }
  // def dependencyDescriptor: Option[DependencyDescriptor] = node.dependencyDescriptor(parent map { _.node })
}

/**
 * CablecarNodeUsage represents dependency from one node to the other.
 * Each rootModuleConf can have different configuration of dependency graph.
 */
private[sbt] case class CablecarNodeUsage(
  // configurations used within rootModuleConf
  configurations: Map[String, Set[String]] = Map(),
  // configurations used by other node conf combo
  requiredConfs: Map[(CablecarNode, String), Set[String]] = Map(), // Should this not use Set() to avoid unstable output?
  // dependers for a given rootModuleConf
  dependers: Map[String, Set[CablecarNodeUsage.Depender]] = Map())

private[sbt] object CablecarNodeUsage {
  case class Depender(dd: DependencyDescriptor, parentConf: String)
}

private[sbt] case class CablecarNode(mrid: ModuleRevisionId, rootOpt: Option[CablecarNode]) {
  def root: CablecarNode = rootOpt getOrElse this
  def isRoot: Boolean = rootOpt.isEmpty
  // TODO
  def isCompletelyEvicted = false
}

private[sbt] object CablecarNode {
  // def apply(md: ModuleDescriptor): CablecarNode = CablecarNode(md.getModuleRevisionId, Some(md), None)
  // def apply(parent: CablecarNode, dd: DependencyDescriptor): CablecarNode = CablecarNode(dd.getDependencyRevisionId, None, Some(parent.root))
}
