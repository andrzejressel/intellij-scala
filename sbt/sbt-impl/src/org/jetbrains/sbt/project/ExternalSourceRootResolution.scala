package org.jetbrains.sbt
package project

import com.intellij.openapi.externalSystem.model.ExternalSystemException
import com.intellij.openapi.externalSystem.model.project.{ExternalSystemSourceType, ModuleData}
import com.intellij.openapi.module.StdModuleTypes
import com.intellij.openapi.util.io.FileUtilRt
import com.intellij.openapi.roots.DependencyScope
import org.jetbrains.plugins.scala.extensions.{ObjectExt, RichFile}
import org.jetbrains.sbt.project.data.{NestedModuleNode, _}
import org.jetbrains.sbt.project.sources.SharedSourcesModuleType
import org.jetbrains.sbt.structure.{ProjectData, ProjectDependencyData}
import org.jetbrains.sbt.{structure => sbtStructure}

import java.io.File
import java.net.URI
import scala.reflect.ClassTag

trait ExternalSourceRootResolution { self: SbtProjectResolver =>

  type ModuleDataNodeType = Node[_<:ModuleData]

  sealed abstract class ModuleSourceSet(val parent: ModuleDataNodeType)
  protected case class PrentModuleSourceSet(override val parent: ModuleDataNodeType) extends ModuleSourceSet(parent)
  protected case class CompleteModuleSourceSet(override val parent: ModuleDataNodeType, main: SbtSourceSetModuleNode, test: SbtSourceSetModuleNode) extends ModuleSourceSet(parent)

  protected def addSharedSourceModules(
    projectToSourceSet: Map[sbtStructure.ProjectData, ModuleSourceSet],
    libraryNodes: Seq[LibraryNode],
    moduleFilesDirectory: File,
    insertProjectTransitiveDependencies: Boolean,
    separateProdTestSources: Boolean,
    buildProjectsGroups: Seq[BuildProjectsGroup]
  ): Unit = {
    val projects = projectToSourceSet.keys.toSeq
    val sharedRoots = sharedAndExternalRootsIn(projects)
    val grouped = groupSharedRoots(sharedRoots)
    val createSourceModule =
      if (separateProdTestSources) {
        createSourceModuleNode(_, castMapValues[CompleteModuleSourceSet](projectToSourceSet), _, _, _)
      } else {
        createSourceModuleNodeLegacy(_, castMapValues[PrentModuleSourceSet](projectToSourceSet), _, _, insertProjectTransitiveDependencies, _)
    }

    grouped.map(createSourceModule(_, libraryNodes, moduleFilesDirectory, buildProjectsGroups))
  }

  protected def createModuleDependencies(
    projectDependencies: Seq[ProjectDependencyData],
    allModules: Seq[ModuleDataNodeType],
    moduleNode: ModuleDataNodeType,
    insertProjectTransitiveDependencies: Boolean
  ): Unit = {
    projectDependencies.foreach { dependencyId =>
      val dependency = allModules
        .find(_.getId == ModuleNode.combinedId(dependencyId.project, dependencyId.buildURI))
        .getOrElse(throw new ExternalSystemException("Cannot find project dependency: " + dependencyId.project))

      val dependencyNode = new ModuleDependencyNode(moduleNode, dependency)
      dependencyNode.setScope(scopeFor(dependencyId.configurations.distinct))
      val exported = if (insertProjectTransitiveDependencies) false else true
      dependencyNode.setExported(exported)
      moduleNode.add(dependencyNode)
    }
  }

  private def castMapValues[R <: ModuleSourceSet : ClassTag](map: Map[sbtStructure.ProjectData, ModuleSourceSet]): Map[sbtStructure.ProjectData, R] =
    map.collect { case (key, value) if value.is[R] =>
        key -> value.asInstanceOf[R]
    }

  private def createSourceModuleNodeLegacy(
    rootGroup: RootGroup,
    projectToModuleNode: Map[sbtStructure.ProjectData, PrentModuleSourceSet],
    libraryNodes: Seq[LibraryNode],
    moduleFilesDirectory: File,
    insertProjectTransitiveDependencies: Boolean,
    buildProjectsGroups: Seq[BuildProjectsGroup]
  ): ModuleDataNodeType = {
    val projects = rootGroup.projects

    val sourceModuleNode = {
      val (moduleNode, contentRootNode) = createSourceModule(rootGroup, moduleFilesDirectory)
      //todo: get jdk from a corresponding jvm module ?
      moduleNode.add(ModuleSdkNode.inheritFromProject)

      // Select a single project and clone its module / project dependencies.
      // It seems that dependencies of any single project should be enough to highlight files in the shared source module.
      // Please note that we mix source modules into other modules on compilation,
      // so source module dependencies are not relevant for compilation, only for highlighting.
      val representativeProject = representativeProjectIn(rootGroup.projects)
      moduleNode.add(createScalaSdkData(representativeProject.scala))

      val representativeProjectDependencies = representativeProject.dependencies

      //add library dependencies of the representative project
      val libraryDependencies = representativeProjectDependencies.modules
      moduleNode.addAll(createLibraryDependencies(libraryDependencies.forProduction)(moduleNode, libraryNodes.map(_.data)))

      //add unmanaged jars/libraries dependencies of the representative project
      val unmanagedLibraryDependencies = representativeProjectDependencies.jars
      moduleNode.addAll(createUnmanagedDependencies(unmanagedLibraryDependencies.forProduction)(moduleNode))

      //add project dependencies of the representative project
      val allSourceModules = projectToModuleNode.values.toSeq.map(_.parent)
      createModuleDependencies(representativeProjectDependencies.projects.forProduction, allSourceModules, moduleNode, insertProjectTransitiveDependencies)

      //add some managed sources of the representative project
      //(see description of `getManagedSourceRootsFromRepresentativeProjectToIncludeAsBaseModelSourceRoots` method for the details)
      val representativeProjectManagedSources = getManagedSourceRootsFromRepresentativeProjectToIncludeAsBaseModelSourceRoots(rootGroup, representativeProject)
      representativeProjectManagedSources.foreach { root =>
        val esSourceType = calculateEsSourceType(root)
        contentRootNode.storePath(esSourceType, root.directory.path)
      }

      projectToModuleNode.get(representativeProject).foreach { case PrentModuleSourceSet(reprProjectModule) =>
        //put source module to the same module group
        extendModuleInternalNameWithGroupName(reprProjectModule, moduleNode)
        //find rootNode for reprProjectModule, because shared sources module should be put in the same root
        val rootNode = findRootNodeForProjectData(representativeProject, buildProjectsGroups, projectToModuleNode)
        rootNode.foreach(_.add(moduleNode))
      }

      moduleNode
    }

    val dependentModulesThatRequireSharedSourcesModule =
      if (insertProjectTransitiveDependencies) {
        getAllModulesThatRequireSharedSourcesModuleLegacy(projectToModuleNode, projects)
      } else {
        Seq.empty
      }

    //add shared sources module as a dependency to platform modules
    val sharedSourceRootProjects = projects.map(projectToModuleNode).map { case PrentModuleSourceSet(module) =>
      (module, DependencyScope.COMPILE)
    }
    val allModulesThatRequireSharedSourcesModule = sharedSourceRootProjects ++ dependentModulesThatRequireSharedSourcesModule
    allModulesThatRequireSharedSourcesModule.foreach { case (ownerModule, dependencyScope) =>
      addModuleDependencyNode(ownerModule, sourceModuleNode, dependencyScope)
    }

    sourceModuleNode
  }

  private def createSourceModuleNode(
    rootGroup: RootGroup,
    projectToSourceSet: Map[sbtStructure.ProjectData, CompleteModuleSourceSet],
    libraryNodes: Seq[LibraryNode],
    moduleFilesDirectory: File,
    buildProjectsGroups: Seq[BuildProjectsGroup]
  ): ModuleDataNodeType = {
    val projects = rootGroup.projects
    val (parentModule, sharedSourcesMainModule, sharedSourcesTestModule) = {
      // note: Select a single project and clone its module / project dependencies.
      // It seems that dependencies of any single project should be enough to highlight files in the shared source module.
      // Please note that we mix source modules into other modules on compilation,
      // so source module dependencies are not relevant for compilation, only for highlighting.
      val representativeProject = representativeProjectIn(rootGroup.projects)
      val CompleteModuleSourceSet(parentModule, mainModule, testModule) =
        createCompleteModuleSourceSetWithAllRequiredData(rootGroup, moduleFilesDirectory, representativeProject, libraryNodes)

      // add project dependencies of the representative project
      val moduleDependencies = representativeProject.dependencies.projects
      val allSourceModules = collectSourceModules(projectToSourceSet)
      Seq((moduleDependencies.forProduction, mainModule), (moduleDependencies.forTest, testModule)).foreach { case(deps, module) =>
        createModuleDependencies(deps, allSourceModules, module, insertProjectTransitiveDependencies = true)
      }

      projectToSourceSet.get(representativeProject).foreach { case CompleteModuleSourceSet(reprProjectModule, _, _) =>
        // put source module to the same module group
        extendModuleInternalNameWithGroupName(reprProjectModule, parentModule, mainModule, testModule)
        // find rootNode for reprProjectModule, because shared sources module should be put in the same root
        val rootNode = findRootNodeForProjectData(representativeProject, buildProjectsGroups, projectToSourceSet)
        rootNode.foreach(_.add(parentModule))
      }

      (parentModule, mainModule, testModule)
    }

    val modulesThatRequireSharedSources = getAllModulesThatRequireSharedSourcesModule(projectToSourceSet, projects)

    modulesThatRequireSharedSources.foreach { case(module, deps) =>
      val sharedSourcesModules = if (deps.project.endsWith("test")) sharedSourcesTestModule else sharedSourcesMainModule
      addModuleDependencyNode(module, sharedSourcesModules, scopeFor(deps.configurations))
    }


    // add shared sources modules as a dependency to platform modules
    projects.map(projectToSourceSet).foreach { case CompleteModuleSourceSet(_, mainModule, testModule) =>
      Seq(
        (mainModule, sharedSourcesMainModule), // put shared sources main module in the platform main module
        (testModule, sharedSourcesMainModule), // put shared sources main module in the platform test module
        (testModule, sharedSourcesTestModule) // put shared sources test module in the platform test module
      ).foreach { case (ownerModule, sharedSourcesModule) =>
        addModuleDependencyNode(ownerModule, sharedSourcesModule, DependencyScope.COMPILE)
      }
    }

    parentModule
  }

  protected def collectSourceModules(projectToSourceSet: Map[sbtStructure.ProjectData, ModuleSourceSet]): Seq[ModuleDataNodeType] =
    projectToSourceSet.values.flatMap {
      case PrentModuleSourceSet(parent) => Seq(parent)
      case CompleteModuleSourceSet(_, main, test) => Seq(main, test)
    }.toSeq

  private def addModuleDependencyNode(ownerModule: ModuleDataNodeType, module: ModuleDataNodeType, dependencyScope: DependencyScope): Unit = {
    val node = new ModuleDependencyNode(ownerModule, module)
    node.setScope(dependencyScope)
    node.setExported(true)
    ownerModule.add(node)
  }

  private def extendModuleInternalNameWithGroupName(
    reprProjectModule: ModuleDataNodeType,
    moduleNodes: ModuleDataNodeType*
  ): Unit = {
    val reprProjectModulePrefix = Option(reprProjectModule.getInternalName.stripSuffix(reprProjectModule.getModuleName))
    moduleNodes.foreach { moduleNode =>
      val moduleNameWithGroupName = prependModuleNameWithGroupName(moduleNode.getInternalName, reprProjectModulePrefix)
      moduleNode.setInternalName(moduleNameWithGroupName)
    }
  }

  private def findRootNodeForProjectData(
    representativeProject: ProjectData,
    buildProjectsGroups: Seq[BuildProjectsGroup],
    projectToModuleNode: Map[sbtStructure.ProjectData, ModuleSourceSet]
  ): Option[ModuleDataNodeType] = {
    val rootProjectDataOpt = buildProjectsGroups
      .find(_.projects.contains(representativeProject))
      .map(_.rootProject)
    rootProjectDataOpt.flatMap(projectToModuleNode.get).map(_.parent)
  }

  /**
   * if project transitive dependencies feature is on, it is required to put shared sources module not only in it's owner module (module with shared sources),
   * but in all modules which depend on modules that have shared resources
   */
  private def getAllModulesThatRequireSharedSourcesModuleLegacy(
    projectToModuleNode: Map[sbtStructure.ProjectData, PrentModuleSourceSet],
    sharedSourcesProjects: Seq[ProjectData]
  ): Seq[(ModuleDataNodeType, DependencyScope)] = {
    projectToModuleNode
      .filterNot { case (project, _) => sharedSourcesProjects.contains(project) }
      .flatMap { case (project, PrentModuleSourceSet(moduleNode)) =>
        val projectsDependentOnSharedSourceProjects = for {
          // note: it is okey to take only production dependencies, because when import without prod/test separation is performed
          // then only production dependencies are filled in
          projectDependencyData <- project.dependencies.projects.forProduction
          sharedSourcesProjectData <- sharedSourcesProjects
          isTheSameSbtProject = Option(sharedSourcesProjectData.buildURI) == projectDependencyData.buildURI
          if isTheSameSbtProject && sharedSourcesProjectData.id == projectDependencyData.project
        } yield projectDependencyData
        Option(projectsDependentOnSharedSourceProjects).filter(_.nonEmpty)
          .map { dependency => (moduleNode, scopeFor(dependency.flatMap(_.configurations))) }
      }.toSeq
  }

  /**
   * if project transitive dependencies feature is on, it is required to put shared sources module not only in it's owner module (module with shared sources),
   * but in all modules which depend on modules that have shared resources
   */
  private def getAllModulesThatRequireSharedSourcesModule(
    projectToModuleNode: Map[ProjectData, CompleteModuleSourceSet],
    sharedSourcesProjects: Seq[ProjectData]
  ): Seq[(SbtSourceSetModuleNode, ProjectDependencyData)] = {

    val sharedSourcesProjectIdMap = sharedSourcesProjects
      .groupBy(_.buildURI)
      .map { case (k, v) => Option(k) -> v }

    def filterOnlyRequiredDependencies(dependencies: Seq[ProjectDependencyData]): Seq[ProjectDependencyData] =
      dependencies
        .filter { projectDependencyData =>
          val sharedSourcesProjects = sharedSourcesProjectIdMap.getOrElse(projectDependencyData.buildURI, Seq.empty)
          val projectName = projectDependencyData.project.dropRight(5)
          sharedSourcesProjects.map(_.id).contains(projectName)
        }

    val moduleToDependencies = projectToModuleNode
      .filterNot { case(project, _) => sharedSourcesProjects.contains(project) }
      .flatMap {case(project, CompleteModuleSourceSet(_, main, test)) => Seq((main, project.dependencies.projects.forProduction), (test, project.dependencies.projects.forTest))}

    moduleToDependencies
      .view.mapValues(filterOnlyRequiredDependencies).toSeq
      .flatMap { case(module, deps) => deps.map((module, _)) }
  }

  /**
   * Selects an arbitrary project, preferable a JVM one
   *
   * Also see [[org.jetbrains.plugins.scala.project.ModuleExt.findRepresentativeModuleForSharedSourceModule]]
   */
  private def representativeProjectIn(projects: Seq[ProjectData]): ProjectData = {
    val isNonJvmTitle = (title: String) => {
      val titleLower = title.toLowerCase()
      titleLower.endsWith("js") || titleLower.endsWith("native")
    }

    val isNonJvmProject = (project: ProjectData) =>
      isNonJvmTitle(project.id) || isNonJvmTitle(project.name)

    //We sort projects by name to have a more deterministic way of how representative projects are picked in cross-build projects
    //If we don't do that, different projects might have dependencies on representative projects with different scala version
    //NOTE: we assume that all subprojects have same prefix and are only different in the suffix
    val projectsSorted = projects.sortBy(_.id)
    val (nonJvmProjects, jvmProjects) = projectsSorted.partition(isNonJvmProject)
    if (jvmProjects.nonEmpty)
      jvmProjects.head
    else
      nonJvmProjects.head
  }

  private def createSourceModule(
    group: RootGroup,
    moduleFilesDirectory: File,
  ): (ModuleDataNodeType, ContentRootNode) = {
    val groupBase = group.base
    val moduleNode = createModuleNode(
      SharedSourcesModuleType.instance.getId,
      group.name,
      group.name,
      moduleFilesDirectory.path,
      groupBase.canonicalPath,
      shouldCreateNestedModule = true
    )

    val contentRootNode = new ContentRootNode(groupBase.path)
    group.roots.foreach { root =>
      val esSourceType = calculateEsSourceType(root)
      contentRootNode.storePath(esSourceType, root.directory.path)
    }

    moduleNode.add(contentRootNode)

    setupOutputDirectories(moduleNode, contentRootNode)

    (moduleNode, contentRootNode)
  }

  protected def createSbtSourceSetModules(
    project: sbtStructure.ProjectData,
    moduleFilesDirectoryPath: String,
    moduleNameWithGroup: String
  ): (SbtSourceSetModuleNode, SbtSourceSetModuleNode) = {
    def sbtSourceSetModule(sourceSetName: String): SbtSourceSetModuleNode = {
      val moduleId = ModuleNode.combinedId(s"${project.id}:$sourceSetName", Option(project.buildURI))
      val moduleNode = new SbtSourceSetModuleNode(
        StdModuleTypes.JAVA.getId,
        moduleId,
        sourceSetName,
        s"$moduleFilesDirectoryPath",
        project.base.canonicalPath
      )
      moduleNode.setInternalName(s"$moduleNameWithGroup.$sourceSetName")
      moduleNode
    }

    (sbtSourceSetModule("main"), sbtSourceSetModule("test"))
  }


  private def createCompleteModuleSourceSetWithAllRequiredData(
    group: RootGroup,
    moduleFilesDirectory: File,
    representativeProject: ProjectData,
    libraryNodes: Seq[LibraryNode]
  ): CompleteModuleSourceSet = {
    val groupPath = group.base.path
    val completeModuleSourceSet = createCompleteModuleSourceSet(group.name, moduleFilesDirectory.path, group.base.canonicalPath)
    val CompleteModuleSourceSet(parentModule, mainModule, testModule) = completeModuleSourceSet

    // add some managed sources of the representative project
    // (see description of `getManagedSourceRootsFromRepresentativeProjectToIncludeAsBaseModelSourceRoots` method for the details)
    val representativeProjectManagedSources = getManagedSourceRootsFromRepresentativeProjectToIncludeAsBaseModelSourceRoots(group, representativeProject).toSeq
    val allRootsToSourceType = (group.roots ++ representativeProjectManagedSources).map(root => (root, calculateEsSourceType(root)))

    // it is not needed to care about excluded because it is not possible to have excluded type see #calculateEsSourceType
    val (testRoots, sourcesRoots) = allRootsToSourceType
      .map { case(root, sourceType) => (root.directory.path, sourceType) }
      .partition { case(_, sourceType) => sourceType.isTest }

    val mainContentRootsData = createContentRootNodes(sourcesRoots, Seq(s"$groupPath/src/main"))
    mainModule.addAll(mainContentRootsData)

    val testContentRootsData = createContentRootNodes(testRoots, Seq(s"$groupPath/src/test"))
    testModule.addAll(testContentRootsData)

    val parentContentRootNode = new ContentRootNode(groupPath)
    parentContentRootNode.storePath(ExternalSystemSourceType.EXCLUDED, getOrCreateTargetDir(groupPath, "target").getAbsolutePath)
    parentModule.add(parentContentRootNode)

    setupOutputDirectoryBasedOnRelPath(mainModule, groupPath, ExternalSystemSourceType.SOURCE, "target/classes")
    setupOutputDirectoryBasedOnRelPath(testModule, groupPath, ExternalSystemSourceType.TEST, "target/test-classes")

    val scalaSdk = createScalaSdkData(representativeProject.scala)
    Seq(parentModule, mainModule, testModule).foreach { module =>
      module.add(ModuleSdkNode.inheritFromProject)
      module.add(scalaSdk)
    }

    val representativeProjectDependencies = representativeProject.dependencies

    //add library dependencies of the representative project
    val libraryDependencies = representativeProjectDependencies.modules
    val librariesNodeData = libraryNodes.map(_.data)
    mainModule.addAll(createLibraryDependencies(libraryDependencies.forProduction)(mainModule, librariesNodeData))
    testModule.addAll(createLibraryDependencies(libraryDependencies.forTest)(testModule, librariesNodeData))

    //add unmanaged jars/libraries dependencies of the representative project
    val unmanagedLibraryDependencies = representativeProjectDependencies.jars
    mainModule.addAll(createUnmanagedDependencies(unmanagedLibraryDependencies.forProduction)(mainModule))
    testModule.addAll(createUnmanagedDependencies(unmanagedLibraryDependencies.forTest)(testModule))

    parentModule.addAll(Seq(mainModule, testModule))

    completeModuleSourceSet
  }

  private def createCompleteModuleSourceSet(
    name: String,
    moduleFilesDirectory: String,
    baseCanonicalPath: String
  ): CompleteModuleSourceSet = {
    def createSbtSourceSetModuleNode(sourceSetName: String) = {
      val fullName = s"$name.$sourceSetName"
      val node = new SbtSourceSetModuleNode(
        SharedSourcesModuleType.instance.getId,
        fullName,
        sourceSetName,
        moduleFilesDirectory,
        baseCanonicalPath
      )
      node.setInternalName(fullName)
      node
    }

    val parent = new NestedModuleNode(
      SharedSourcesModuleType.instance.getId,
      name,
      name,
      moduleFilesDirectory,
      baseCanonicalPath
    )
    CompleteModuleSourceSet(parent, createSbtSourceSetModuleNode("main"), createSbtSourceSetModuleNode("test"))
  }

  protected def createContentRootNodes(
    roots: Seq[(String, ExternalSystemSourceType)],
    rootPaths: Seq[String]
  ): Seq[ContentRootNode] = {
    val contentRootNodes = rootPaths.distinct.map(path => new ContentRootNode(path))
    roots.foldLeft(contentRootNodes){ (nodes, curr) =>
      val (root, sourceType) = curr
      val suitableContentRootNode = nodes.find(node => new File(root).isUnder(new File(node.data.getRootPath)))
      suitableContentRootNode match {
        case Some(contentRootNode) =>
          contentRootNode.storePath(sourceType, root)
          nodes
        case None =>
          nodes :+ new ContentRootNode(root)
      }
    }
  }


  /**
   * The primary use case for this logic is to handle SBT projects with `projectmatrix` sbt plugin.<br>
   * You can inspect `sbt-projectmatrix-with-source-generators` test project as an example.
   *
   * Details:<br>
   * In sbt build with `projectmatrix` sbt plugin, for a single project multiple subprojects are generated
   * For example if we define single project {{{
   *     val downstream = (projectMatrix in file("downstream"))
   *         .settings(commonSettings(false) *)
   *         .jvmPlatform(scalaVersions = Seq("2.12.17", "2.13.10"))
   *         .jsPlatform(scalaVersions = Seq("2.12.17", "2.13.10"))
   * }}}
   * 4 extra subprojects will be generated (2 JVM projects with 2 scala versions and 2 JS projects with 2 scala version)
   *
   * But generated sources for such projects will be located outside their base directory (or "contentRoot" in terms of IDEA)
   * Instead, they will be located in the content root of the original project, but in a special folders, like: {{{
   *     target/jvm-2.12/src_managed/main
   *     target/jvm-2.13/src_managed/main
   *     target/js-2.12/src_managed/main
   *     target/js-2.13/src_managed/main
   * }}}
   * So they will not be registered as source roots for IntelliJ Module (source roots must be located under the content root).
   * That's why we need to explicitly add source dependency from the representative project, by analogy with it's module/library/jars dependencies
   *
   * In case some logic is not clear, try to comment it out and run project structure/highlighting tests
   */
  private def getManagedSourceRootsFromRepresentativeProjectToIncludeAsBaseModelSourceRoots(
    rootGroup: RootGroup,
    representativeProject: ProjectData
  ): Set[Root] = {
    val rootGroupBase = rootGroup.base
    val representativeProjectBase = representativeProject.base

    val sourceRootsFromRepresentative: Seq[Root] = sourceRootsIn(representativeProject)
    sourceRootsFromRepresentative
      .filter(_.managed)
      .toSet
      //ensure that source roots are not already listed in root group roots to avoid duplicates
      .diff(rootGroup.roots.toSet)
      //ensure that source roots are in the content root of base module
      .filter(_.directory.isUnder(rootGroupBase))
      //get those source roots which are outside representative project content root
      .filterNot(_.directory.isUnder(representativeProjectBase))
  }

  //target directory are expected by jps compiler:
  //if they are missing all sources are marked dirty and there is no incremental compilation
  private def setupOutputDirectories(moduleNode: ModuleDataNodeType, contentRootNode: ContentRootNode): Unit = {
    val contentRoot = contentRootNode.data.getRootPath

    Seq((ExternalSystemSourceType.EXCLUDED, "target"), (ExternalSystemSourceType.SOURCE, "target/classes"), (ExternalSystemSourceType.TEST, "target/test-classes"))
      .foreach { case (sourceType, relPath) =>
        setupOutputDirectoryBasedOnRelPath(moduleNode, contentRoot, sourceType, relPath)
      }
  }

  private def setupOutputDirectoryBasedOnRelPath(
    moduleNode: ModuleDataNodeType,
    basePath: String,
    sourceType: ExternalSystemSourceType,
    relPath: String
  ): Unit = {
    moduleNode.setInheritProjectCompileOutputPath(false)
    moduleNode.setCompileOutputPath(sourceType, getOrCreateTargetDir(basePath, relPath).getAbsolutePath)
  }

  private def getOrCreateTargetDir(root: String, relPath: String): File = {
    val file = new File(root, relPath)

    if (!file.exists()) {
      FileUtilRt.createDirectory(file)
    }

    file
  }

  private def calculateEsSourceType(root: Root): ExternalSystemSourceType =
    ExternalSystemSourceType.from(
      root.scope == Root.Scope.Test,
      root.managed,
      root.kind == Root.Kind.Resources,
      false
    )

  private def sharedAndExternalRootsIn(projects: Seq[sbtStructure.ProjectData]): Seq[SharedRoot] = {
    val projectRoots = projects.flatMap(project => sourceRootsIn(project).map(ProjectRoot(project,_)))

    // TODO return the message about omitted directories
    val internalSourceDirectories = projectRoots.filter(_.isInternal).map(_.root.directory)

    projectRoots
      .filter(it => it.isExternal && !internalSourceDirectories.contains(it.root.directory))
      .groupBy(_.root)
      .view.mapValues(_.map(_.project).toSet).toMap
      .map(p => SharedRoot(p._1, p._2.toSeq))
      .toSeq
  }

  private def groupSharedRoots(roots: Seq[SharedRoot]): Seq[RootGroup] = {
    val nameProvider = new SharedSourceRootNameProvider()

    // TODO consider base/projects correspondence
    val rootsGroupedByBase = roots.groupBy(_.root.basePathFromKnownHardcodedDefaultPaths)
    rootsGroupedByBase.toList.collect {
      //NOTE: ignore roots with empty base to avoid dangling "shared-sources" module
      case (Some(base), sharedRoots) =>
        val name = nameProvider.nameFor(base)
        val projects = sharedRoots.flatMap(_.projects).distinct
        RootGroup(name, sharedRoots.map(_.root), projects)
    }
  }

  private def sourceRootsIn(project: sbtStructure.ProjectData): Seq[Root] = {
    val relevantScopes = Set("compile", "test", "it")

    val relevantConfigurations = project.configurations.filter(it => relevantScopes.contains(it.id))

    relevantConfigurations.flatMap { configuration =>
      def createRoot(kind: Root.Kind)(directory: sbtStructure.DirectoryData): Root = {
        val scope = if (configuration.id == "compile") Root.Scope.Compile else Root.Scope.Test
        Root(scope, kind, directory.file.canonicalFile, directory.managed)
      }

      val sourceRoots = configuration.sources.map(createRoot(Root.Kind.Sources))
      val resourceRoots = configuration.resources.map(createRoot(Root.Kind.Resources))
      sourceRoots ++ resourceRoots
    }
  }

  /**
   * This class is designed to group projects from single SBT build.
   * Note, SBT single sbt build can consists from multiple other builds using `ProjectRef`
   *
   * @param buildUri can point to a directory ot a github repository
   */
  protected case class BuildProjectsGroup(
    buildUri: URI,
    rootProject: ProjectData,
    projects: Seq[ProjectData],
    rootProjectModuleNameUnique: String,
  )

  private case class RootGroup(name: String, roots: Seq[Root], projects: Seq[sbtStructure.ProjectData]) {
    lazy val base: File = commonBase(roots)

    private def commonBase(roots: Seq[Root]): File = {
      import scala.jdk.CollectionConverters._
      val paths = roots.map { root =>
        root.basePathFromKnownHardcodedDefaultPaths.getOrElse(root.directory)
          .getCanonicalFile.toPath.normalize
      }

      paths.foldLeft(paths.head) { case (common, it) =>
        common.iterator().asScala.zip(it.iterator().asScala)
            .takeWhile { case (c,p) => c==p}
            .map(_._1)
            .foldLeft(paths.head.getRoot) { case (base,child) => base.resolve(child)}
      }.toFile
    }
  }

  private case class SharedRoot(root: Root, projects: Seq[sbtStructure.ProjectData])

  private case class ProjectRoot(project: sbtStructure.ProjectData, root: Root) {
    def isInternal: Boolean = !isExternal

    def isExternal: Boolean = root.directory.isOutsideOf(project.base)
  }

  private case class Root(
    scope: Root.Scope,
    kind: Root.Kind,
    directory: File,
    managed: Boolean
  ) {
    lazy val basePathFromKnownHardcodedDefaultPaths: Option[File] = Root.DefaultPaths.collectFirst {
      //Example directory: /c/example-project/downstream/src/test/java (check if it parent ends with `src/test`)
      case paths if directory.parent.exists(_.endsWith(paths: _*)) => directory << (paths.length + 1)
    }
  }

  private object Root {
    private val DefaultPaths = Seq(
      Seq("src", "main"),
      Seq("src", "test"),
    )

    sealed trait Scope
    object Scope {
      case object Compile extends Scope
      case object Test extends Scope
    }

    sealed trait Kind
    object Kind {
      case object Sources extends Kind
      case object Resources extends Kind
    }
  }

  private class SharedSourceRootNameProvider {
    private var usedNames = Set.empty[String]
    private var counter = 1

    def nameFor(base: File): String = {
      val namedDirectory = if (base.getName == "shared") base.parent.getOrElse(base) else base
      val prefix = s"${namedDirectory.getName}-sources"

      val result = if (usedNames.contains(prefix)) {
        counter += 1
        s"$prefix-$counter"
      } else {
        prefix
      }

      usedNames += result
      result
    }
  }

  protected def prependModuleNameWithGroupName(moduleName: String, group: Option[String]): String = {
    val moduleNameWithGroupPrefix = group
      .filterNot(_.isBlank)
      // the group name might ended with a dot, when it is from org/jetbrains/sbt/project/ExternalSourceRootResolution.scala:111
      // and can be without a dot, when it is from org.jetbrains.sbt.project.SbtProjectResolver#createModuleWithAllRequiredData
      .map(groupName => if (groupName.endsWith(".")) groupName else s"$groupName.")
      .map(_ + moduleName)

    moduleNameWithGroupPrefix.getOrElse(moduleName)
  }

  protected def createModuleNode(
    typeId: String,
    projectId: String,
    moduleName: String,
    moduleFileDirectoryPath: String,
    externalConfigPath: String,
    shouldCreateNestedModule: Boolean
  ): ModuleDataNodeType = {
    if (shouldCreateNestedModule) {
      new NestedModuleNode(typeId, projectId, moduleName, moduleFileDirectoryPath, externalConfigPath)
    } else {
      new ModuleNode(typeId, projectId, moduleName, moduleFileDirectoryPath, externalConfigPath)
    }
  }

}
