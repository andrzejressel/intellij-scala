package org.jetbrains.sbt.project

import com.intellij.compiler.impl.javaCompiler.javac.JavacConfiguration
import com.intellij.openapi.externalSystem.util.{DisposeAwareProjectChange, ExternalSystemApiUtil}
import com.intellij.openapi.projectRoots.ProjectJdkTable
import com.intellij.openapi.roots.DependencyScope
import com.intellij.pom.java.LanguageLevel
import com.intellij.testFramework.IdeaTestUtil
import org.junit.Assert.{assertEquals, assertTrue}
import org.jetbrains.annotations.Nullable
import org.jetbrains.jps.model.java.compiler.JpsJavaCompilerOptions
import org.jetbrains.plugins.scala.SlowTests
import org.jetbrains.plugins.scala.compiler.data.CompileOrder
import org.jetbrains.plugins.scala.extensions.{RichFile, inWriteAction}
import org.jetbrains.plugins.scala.project.ProjectExt
import org.jetbrains.plugins.scala.project.external.JdkByName
import org.jetbrains.sbt.Sbt
import org.junit.Assert
import org.junit.experimental.categories.Category

import java.net.URI
import scala.annotation.nowarn

@Category(Array(classOf[SlowTests]))
final class SbtProjectStructureImportingTest extends SbtProjectStructureImportingLike {

  import ProjectStructureDsl._

  def testSimple(): Unit = {
    val scalaLibraries = ProjectStructureTestUtils.expectedScalaLibraryWithScalaSdk("2.13.5")
    runSimpleTest("simple", scalaLibraries)

    // Adding the assertion here not to create a separate heavy test for such a tiny check
    // org.jetbrains.plugins.scala.project.ProjectExt#modulesWithScala
    Assert.assertEquals(
      "modulesWithScala should return list of non *-build modules",
      Seq("simple"),
      myProject.modulesWithScala.map(_.getName),
    )

    val expectedLineInProcessOutput = "[error] Some error message which shouldn't fail the whole build, see SCL-21478 and SCL-13038"
    assertTrue(
      s"Can't find this line in sbt process output during sbt structure extraction:\n$expectedLineInProcessOutput",
      SbtProjectResolver.processOutputOfLatestStructureDump.contains(expectedLineInProcessOutput)
    )
  }

  //noinspection RedundantDefaultArgument
  def testSimple_Scala3(): Unit = {
    val scalaLibraries = ProjectStructureTestUtils.expectedScalaLibrary("2.13.6") +: ProjectStructureTestUtils.expectedScalaLibraryWithScalaSdk("3.0.2")
    runSimpleTest("simple-scala3", scalaLibraries, DefaultSbtContentRootsScala3)
  }

  private def runSimpleTest(
    projectName: String,
    expectedScalaLibraries: Seq[library],
    expectedSbtCompletionVariants: Seq[ExpectedDirectoryCompletionVariant] = DefaultSbtContentRootsScala213
  ): Unit = {
    runTest(
      new project(projectName) {
        libraries := expectedScalaLibraries

        modules := Seq(
          new module(projectName) {
            contentRoots += getProjectPath
            ProjectStructureDsl.sources := Seq("src/main/scala", "src/main/java")
            testSources := Seq("src/test/scala", "src/test/java")
            resources := Seq("src/main/resources")
            testResources := Seq("src/test/resources")
            excluded := Seq("target")
            libraryDependencies := expectedScalaLibraries
          },
          new module(s"$projectName.$projectName-build") {
            ProjectStructureDsl.sources := Seq("")
            excluded := Seq("project/target", "target")
          }
        )
      }
    )

    val projectBaseDir = myProject.baseDir
    assertSbtDirectoryCompletionContributorVariants(
      projectBaseDir,
      expectedSbtCompletionVariants
    )
  }

  // note: this test is for the case in which an additional project is linked to the project.
  // The linked project is project "simple". The ideProject is generated from "twoLinkedProjects" project
  def testTwoLinkedProjects(): Unit = {
    val originalProjectName = "twoLinkedProjects"
    val linkedProjectName = "simple"
    val expectedScalaLibraries = ProjectStructureTestUtils.expectedScalaLibraryWithScalaSdk("2.13.5")
    val linkedSbtProjectPath = generateTestProjectPath(linkedProjectName)
    linkSbtProject(linkedSbtProjectPath, prodTestSourcesSeparated = false)
    runTest(
      new project("testTwoLinkedProjects") {
        modules := Seq(
          new module(originalProjectName) {
            contentRoots += getProjectPath
            excluded := Seq("target")
            sources := Seq("src/main/scala", "src/main/java")
            resources := Seq("src/main/resources")
            testSources := Seq("src/test/scala", "src/test/java")
            testResources := Seq("src/test/resources")
            libraryDependencies := expectedScalaLibraries
          },
          new module(s"$originalProjectName.$originalProjectName-build") {
            ProjectStructureDsl.sources := Seq("")
            excluded := Seq("project/target", "target")
          },
          new module(linkedProjectName) {
            contentRoots += linkedSbtProjectPath
            excluded := Seq("target")
            sources := Seq("src/main/scala", "src/main/java")
            resources := Seq("src/main/resources")
            testSources := Seq("src/test/scala", "src/test/java")
            testResources := Seq("src/test/resources")
            libraryDependencies := expectedScalaLibraries
          },
          new module(s"$linkedProjectName.$linkedProjectName-build") {
            sources := Seq("")
            excluded := Seq("project/target", "target")
          }
        )
      }
    )
    Seq(linkedSbtProjectPath, getProjectPath).foreach { path =>
      assertSbtDirectoryCompletionContributorVariants(
        findVirtualFile(path),
        DefaultSbtContentRootsScala213
      )
    }
  }

  def testProjectWithUppercaseName(): Unit = runTest {
    new project("MyProjectWithUppercaseName") {
      lazy val scalaLibraries: Seq[library] = ProjectStructureTestUtils.expectedScalaLibraryWithScalaSdk("2.13.6")
      libraries ++= scalaLibraries

      modules := Seq(
        new module("MyProjectWithUppercaseName") {
          libraryDependencies ++= scalaLibraries
        },
        new module("MyProjectWithUppercaseName.MyProjectWithUppercaseName-build") {
        }
      )
    }
  }

  def testSimpleDoNotUseCoursier(): Unit = {
    val scalaLibraries = ProjectStructureTestUtils.expectedScalaLibraryWithScalaSdkFromIvy("2.12.10")
    runSimpleTest("simpleDoNotUseCoursier", scalaLibraries, DefaultSbtContentRootsScala212)
  }

  def testMultiModule(): Unit = runTest(
    new project("multiModule") {
      lazy val foo: module = new module("multiModule.foo") {
        moduleDependencies += new dependency(bar) {
          isExported := false
        }
      }

      lazy val bar  = new module("multiModule.bar")
      lazy val root = new module("multiModule")

      modules := Seq(root, foo, bar)
    }
  )

  def testUnmanagedDependency(): Unit = runTest(
    new project("unmanagedDependency") {
      val scalaLibraries: Seq[library] = ProjectStructureTestUtils.expectedScalaLibraryWithScalaSdk("2.13.6")
      val managedLibrary: library = new library("sbt: org.apache.commons:commons-compress:1.21:jar")
      libraries := scalaLibraries :+ managedLibrary

      modules += new module("unmanagedDependency") {
        lazy val unmanagedLibrary: library = new library(s"sbt: ${Sbt.UnmanagedLibraryName}") {
          libClasses += (getTestProjectDir / "lib" / "unmanaged.jar").getAbsolutePath
        }

        libraries := Seq(unmanagedLibrary)
        val myLibraryDependencies: Seq[library] = unmanagedLibrary +: managedLibrary +: scalaLibraries
        libraryDependencies := myLibraryDependencies
      }
    }
  )

  def testSharedSources(): Unit = runTest(
    new project("sharedSourcesProject") {
      lazy val scalaLibraries: Seq[library] = ProjectStructureTestUtils.expectedScalaLibraryWithScalaSdk("2.13.6")
      libraries := scalaLibraries

      lazy val root: module = new module("sharedSourcesProject") {
        contentRoots := Seq(getProjectPath)
        sources := Seq("src/main/scala")
        libraryDependencies := scalaLibraries
        moduleDependencies := Nil
      }

      lazy val sharedSourcesModule: module = new module("sharedSourcesProject.sharedSources-sources") {
        contentRoots := Seq(getProjectPath + "/shared")
        libraryDependencies := scalaLibraries
        sources := Seq("src/main/scala")
      }

      lazy val foo: module = new module("sharedSourcesProject.foo") {
        libraryDependencies := scalaLibraries
        moduleDependencies := Seq(
          new dependency(sharedSourcesModule) { isExported := true }
        )
      }

      lazy val bar: module = new module("sharedSourcesProject.bar") {
        libraryDependencies := scalaLibraries
        moduleDependencies := Seq(
          new dependency(sharedSourcesModule) { isExported := true }
        )
      }

      modules := Seq(root, foo, bar, sharedSourcesModule)
    }
  )

  def testSbtIdeSettingsRespectIdeExcludedDirectoriesSetting(): Unit = runTest(
    new project("root") {
      modules += new module("root") {
        excluded := Seq(
          "directory-to-exclude-1",
          "directory/to/exclude/2"
        )
      }
    }
  )

  def testSharedSourcesWithNestedProjectDependencies(): Unit = runTest(
    new project("sharedSourcesWithNestedProjectDependencies") {
      lazy val scalaLibraries: Seq[library] = ProjectStructureTestUtils.expectedScalaLibraryWithScalaSdk("2.13.6")
      libraries := scalaLibraries

      lazy val root: module = new module("sharedSourcesWithNestedProjectDependencies") {
        contentRoots := Seq(getProjectPath)
        sources := Seq("src/main/scala")
        libraryDependencies := scalaLibraries
        moduleDependencies := Seq(
          new dependency(sharedSourcesModule) {
            isExported := true
            scope := DependencyScope.COMPILE
          },
          new dependency(bar) {
            isExported := false
            scope := DependencyScope.COMPILE
          },
          new dependency(dummy) {
            isExported := false
            scope := DependencyScope.COMPILE
          },
        )
      }

      lazy val sharedSourcesModule: module = new module("sharedSourcesWithNestedProjectDependencies.sharedSourcesWithNestedProjectDependencies-sources") {
        contentRoots := Seq(getProjectPath + "/shared")
        libraryDependencies := scalaLibraries
        sources := Seq("src/main/scala")
      }

      lazy val foo: module = new module("sharedSourcesWithNestedProjectDependencies.foo") {
        libraryDependencies := scalaLibraries
        moduleDependencies := Seq(
          new dependency(sharedSourcesModule) {
            isExported := true
          }
        )
      }

      lazy val bar: module = new module("sharedSourcesWithNestedProjectDependencies.bar") {
        libraryDependencies := scalaLibraries
        moduleDependencies := Seq(
          new dependency(sharedSourcesModule) {
            isExported := true
          }
        )
      }

      lazy val dummy: module = new module("sharedSourcesWithNestedProjectDependencies.dummy") {
        libraryDependencies := scalaLibraries
        moduleDependencies := Seq(
          new dependency(sharedSourcesModule) {
            isExported := true
          },
          new dependency(bar) {
            isExported := false
            scope := DependencyScope.COMPILE
          },
        )
      }

      modules := Seq(root, foo, bar, dummy, sharedSourcesModule)
    }
  )

  /**
   * SCL-12520: Generate a shared sources module when it is only used from a single other module
   */
  def testSCL12520(): Unit = runTest(
    new project("scl12520") {
      val sharedModule: module = new module("scl12520.p1-sources") {
        contentRoots += getProjectPath + "/p1/shared"
      }

      val jvmModule: module = new module("scl12520.p1") {
        moduleDependencies += new dependency(sharedModule) {
          isExported := true
        }
        contentRoots += getProjectPath + "/p1/jvm"
      }

      val rootModule: module = new module("scl12520") {}
      val rootBuildModule: module = new module("scl12520.scl12520-build") {}

      modules := Seq(sharedModule, rootModule, rootBuildModule, jvmModule)
    }
  )

  /**
   * SCL-13600: generate all modules when there is a duplicate project id in the sbt build
   * due to references to different builds, or multiple sbt projects being imported independently from IDEA
   */
  def testSCL13600(): Unit = runTest(
    new project("root") {
      val buildURI: URI = getTestProjectDir.getCanonicalFile.toURI

      val rootC1: module = new module("Build C1 Name") {
        sbtProjectId := "root"
        sbtBuildURI := buildURI.resolve("c1/")
        moduleDependencies := Seq()
      }
      val rootC2: module = new module("Build C2 Name") {
        sbtProjectId := "root"
        sbtBuildURI := buildURI.resolve("c2/")
        moduleDependencies := Seq()
      }
      val rootC3: module = new module("root~1") {
        sbtProjectId := "root"
        sbtBuildURI := buildURI.resolve("prefix1/prefix2/c3/suffix1/suffix2/")
        moduleDependencies := Seq()
      }
      val rootC4: module = new module("root~2") {
        sbtProjectId := "root"
        sbtBuildURI := buildURI.resolve("prefix1/prefix2/c4/suffix1/suffix2/")
        moduleDependencies := Seq()
      }
      val root: module = new module("root") {
        sbtProjectId := "root"
        sbtBuildURI := buildURI
        moduleDependencies := Seq(
          new dependency(rootC1) {
            isExported := false
          },
          new dependency(rootC2) {
            isExported := false
          },
          new dependency(rootC3) {
            isExported := false
          },
          new dependency(rootC4) {
            isExported := false
          },
        )
      }

      val modulesFromRoot: Seq[module] = Seq(
        new module("project1InRootBuild", Array("root")),
        new module("project2InRootBuild", Array("root")),
        new module("project3InRootBuildWithSameName", Array("root", "same name in root build")),
        new module("project4InRootBuildWithSameName", Array("root", "same name in root build")),
        new module("project5InRootBuildWithSameGlobalName", Array("root", "same global name")),
        new module("project6InRootBuildWithSameGlobalName", Array("root", "same global name")),
      )
      val modulesFromC1: Seq[module] = Seq(
        rootC1,
        new module("project1InC1", Array("Build C1 Name")),
        new module("project2InC1", Array("Build C1 Name")),
        new module("project3InC1WithSameName", Array("Build C1 Name", "same name in c1")),
        new module("project4InC1WithSameName", Array("Build C1 Name", "same name in c1")),
        new module("project5InC1WithSameGlobalName", Array("Build C1 Name", "same global name")),
        new module("project6InC1WithSameGlobalName", Array("Build C1 Name", "same global name")),
      )
      val modulesFromC2: Seq[module] = Seq(
        rootC2,
        new module("project1InC2", Array("Build C2 Name")),
        new module("project2InC2", Array("Build C2 Name")),
        new module("project3InC2WithSameName", Array("Build C2 Name", "same name in c2")),
        new module("project4InC2WithSameName", Array("Build C2 Name", "same name in c2")),
        new module("project5InC2WithSameGlobalName", Array("Build C2 Name", "same global name")),
        new module("project6InC2WithSameGlobalName", Array("Build C2 Name", "same global name")),
      )
      val modulesFromC3: Seq[module] = Seq(
        rootC3,
        new module("project1InC3", Array("root~1")),
        new module("project2InC3", Array("root~1")),
        new module("project3InC3WithSameName", Array("root~1", "same name in c3")),
        new module("project4InC3WithSameName", Array("root~1", "same name in c3")),
        new module("project5InC3WithSameGlobalName", Array("root~1", "same global name")),
        new module("project6InC3WithSameGlobalName", Array("root~1", "same global name")),
      )
      val modulesFromC4: Seq[module] = Seq(
        rootC4,
        new module("project1InC4", Array("root~2")),
        new module("project2InC4", Array("root~2")),
        new module("project3InC4WithSameName", Array("root~2", "same name in c4")),
        new module("project4InC4WithSameName", Array("root~2", "same name in c4")),
        new module("project5InC4WithSameGlobalName", Array("root~2", "same global name")),
        new module("project6InC4WithSameGlobalName", Array("root~2", "same global name")),
      )

      modules := root +:
        modulesFromRoot ++:
        modulesFromC1 ++:
        modulesFromC2 ++:
        modulesFromC3 ++:
        modulesFromC4
    }
  )

  def testSCL14635(): Unit = runTest(
    new project("SCL-14635") {
      private val buildURI: URI = getTestProjectDir.getCanonicalFile.toURI

      private val sbtIdeaPluginGroup = Array("sbtIdeaPlugin")
      private val sbtIdeaShellGroup = Array("sbt-idea-shell")
      private val sbtIdeSettingsGroup = Array("sbt-ide-settings")

      // NOTE: sbtIdeaPlugin also has inner module named `sbt-idea-plugin` (with dashes), but it's separate, non-root module
      val sbtIdeaPlugin = new module("sbtIdeaPlugin") {
        sbtBuildURI := new URI("https://github.com/JetBrains/sbt-idea-plugin.git")
        sbtProjectId := "sbtIdeaPlugin"
      }

      val sbtIdeaShell = new module("sbt-idea-shell") {
        sbtBuildURI := new URI("https://github.com/JetBrains/sbt-idea-shell.git#master")
        sbtProjectId := "root"
      }

      val sbtIdeSettings = new module("sbt-ide-settings") {
        sbtBuildURI := new URI("https://github.com/JetBrains/sbt-ide-settings.git")
        sbtProjectId := "sbt-ide-settings"
      }


      modules := Seq(
        new module("SCL-14635") {
          sbtBuildURI := buildURI
          sbtProjectId := "root"
          moduleDependencies := Seq(
            new dependency(sbtIdeaPlugin) {
              isExported := false
              scope := DependencyScope.COMPILE
            },
            new dependency(sbtIdeaShell) {
              isExported := false
              scope := DependencyScope.COMPILE
            },
            new dependency(sbtIdeSettings) {
              isExported := false
              scope := DependencyScope.COMPILE
            },
          )
        },
        new module("SCL-14635-build", Array("SCL-14635")),
        sbtIdeaPlugin,
        new module("sbt-idea-plugin", sbtIdeaPluginGroup),
        new module("sbt-declarative-core", sbtIdeaPluginGroup),
        new module("sbt-declarative-packaging", sbtIdeaPluginGroup),
        new module("sbt-declarative-visualizer", sbtIdeaPluginGroup),
        new module("sbtIdeaPlugin-build", sbtIdeaPluginGroup),
        sbtIdeaShell,
        new module("sbt-idea-shell-build", sbtIdeaShellGroup),
        sbtIdeSettings,
        new module("sbt-ide-settings-build", sbtIdeSettingsGroup)
      )
    }
  )

  def testNonSourceConfigurationsWithNestedProjectDependencies():Unit = {
    val projectName = "nonSourceConfigurationsWithNestedProjectDependencies"
    runTest(
      new project(projectName) {

        lazy val proj0: module = new module(s"$projectName.proj0") {
          sbtProjectId := "proj0"
          moduleDependencies := Seq()
        }

        lazy val proj1: module = new module(s"$projectName.proj1") {
          sbtProjectId := "proj1"
          moduleDependencies := Seq(
            new dependency(proj0) {
              isExported := false
              scope := DependencyScope.TEST
            }
          )
        }

        lazy val proj2: module = new module(s"$projectName.proj2") {
          sbtProjectId := "proj2"
          moduleDependencies := Seq(
            new dependency(proj0) {
              isExported := false
              scope := DependencyScope.PROVIDED
            },
            new dependency(proj1) {
              isExported := false
              scope := DependencyScope.PROVIDED
            }
          )
        }

        lazy val proj3: module = new module(s"$projectName.proj3") {
          sbtProjectId := "proj3"
          moduleDependencies := Seq(
            new dependency(proj0) {
              isExported := false
              scope := DependencyScope.COMPILE
            },
            new dependency(proj1) {
              isExported := false
              scope := DependencyScope.COMPILE
            }
          )
        }

        lazy val root: module = new module(projectName) {
          sbtProjectId := "root"
          moduleDependencies := Seq(
            new dependency(proj2) {
              isExported := false
              scope := DependencyScope.TEST
            }
          )
        }
        modules := Seq(root, proj0, proj1, proj2, proj3)
      }
    )
  }

  def testCrossPlatformWithNestedProjectDependencies(): Unit = {
    val projectName = "crossPlatformWithNestedProjectDependencies"
    runTest(
      new project(projectName) {

        lazy val module1JS = new module("module1JS", Array(projectName, "module1"))
        lazy val module1JVM = new module("module1JVM", Array(projectName, "module1"))
        lazy val module1Sources = new module("module1-sources", Array(projectName, "module1"))

        lazy val module2JS = new module("module2JS", Array(projectName, "module2")){
          moduleDependencies := Seq(
            new dependency(module1JS) {
              isExported := false
              scope := DependencyScope.TEST
            },
            new dependency(module1Sources) {
              isExported := true
              scope := DependencyScope.TEST
            },
            new dependency(module2Sources) {
              isExported := true
              scope := DependencyScope.COMPILE
            },
          )
        }
        lazy val module2JVM = new module("module2JVM", Array(projectName, "module2")) {
          moduleDependencies := Seq(
            new dependency(module1JVM) {
              isExported := false
              scope := DependencyScope.TEST
            },
            new dependency(module1Sources) {
              isExported := true
              scope := DependencyScope.TEST
            },
            new dependency(module2Sources) {
              isExported := true
              scope := DependencyScope.COMPILE
            },
          )
        }
        lazy val module2Sources = new module("module2-sources", Array(projectName, "module2")) {
          moduleDependencies := Seq(
            new dependency(module1JVM) {
              isExported := false
              scope := DependencyScope.TEST
            }
          )
        }

        lazy val module3 = new module(s"$projectName.module3") {
          moduleDependencies := Seq(
            new dependency(module2JVM) {
              isExported := false
              scope := DependencyScope.TEST
            },
            new dependency(module2Sources) {
              isExported := true
              scope := DependencyScope.TEST
            },
            new dependency(module1JVM) {
              isExported := false
              scope := DependencyScope.TEST
            },
            new dependency(module1Sources) {
              isExported := true
              scope := DependencyScope.TEST
            }
          )
        }

        lazy val root = new module(projectName) {
          sbtProjectId := "root"
          moduleDependencies := Seq(
            new dependency(module2JVM) {
              isExported := false
              scope := DependencyScope.COMPILE
            },
            new dependency(module2Sources) {
              isExported := true
              scope := DependencyScope.COMPILE
            }
          )
        }

        modules := Seq(root, module1JS, module1JVM, module1Sources, module2JS, module2JVM, module2Sources, module3)
      }
    )
  }

  //noinspection TypeAnnotation
  // SCL-16204, SCL-17597
  def testJavaLanguageLevelAndTargetByteCodeLevel(): Unit = {
    //overriding project jdk (configured in base test class)
    val projectSdk9 = IdeaTestUtil.getMockJdk9
    inWriteAction {
      ProjectJdkTable.getInstance.addJdk(projectSdk9)
    }
    getCurrentExternalProjectSettings.jdk = projectSdk9.getName

    //sbt can't be run with mock project JDK, so ensure it has normal SDK (configured in base test class)
    setSbtSettingsCustomSdk(getJdkConfiguredForTestCase)

    val projectName = "java-language-level-and-target-byte-code-level"
    try runTest(
      new project(projectName) {
        // we expect no other options except -source -target --release or --enable-preview in this test
        // these options are specially handled and saved in the dedicated settings, so we don't expect any extra javacOptions
        javacOptions := Nil
        sdk := JdkByName(projectSdk9.getName)

        def moduleX(name: String, source: LanguageLevel, @Nullable target: String): module = new module(name) {
          javaLanguageLevel := source
          javaTargetBytecodeLevel := target
          javacOptions := Nil
          sdk := JdkByName(projectSdk9.getName)
        }

        val sdkLanguageLevel: LanguageLevel = LanguageLevel.JDK_1_9

        val root = moduleX("java-language-level-and-target-byte-code-level", sdkLanguageLevel, null)

        // Module naming: `source_target_release`
        // `x` means option is missing
        val module_x_x_x = moduleX(s"$projectName.module_x_x_x", sdkLanguageLevel, null)

        val module_8_8_x   = moduleX(s"$projectName.module_8_8_x", LanguageLevel.JDK_1_8, "8")
        val module_8_11_x  = moduleX(s"$projectName.module_8_11_x", LanguageLevel.JDK_1_8, "11")
        val module_11_8_x  = moduleX(s"$projectName.module_11_8_x", LanguageLevel.JDK_11, "8")
        val module_11_11_x = moduleX(s"$projectName.module_11_11_x", LanguageLevel.JDK_11, "11")

        // no explicit target: javac will use source level by default
        val module_8_x_x  = moduleX(s"$projectName.module_8_x_x", LanguageLevel.JDK_1_8, null)
        val module_11_x_x = moduleX(s"$projectName.module_11_x_x", LanguageLevel.JDK_11, null)
        val module_14_x_x = moduleX(s"$projectName.module_14_x_x", LanguageLevel.JDK_14, null)
        val module_15_x_x = moduleX(s"$projectName.module_15_x_x", LanguageLevel.JDK_15, null)

        val module_x_8_x  = moduleX(s"$projectName.module_x_8_x", sdkLanguageLevel, "8")
        val module_x_11_x = moduleX(s"$projectName.module_x_11_x", sdkLanguageLevel, "11")

        val module_x_x_8  = moduleX(s"$projectName.module_x_x_8", LanguageLevel.JDK_1_8, "8")
        val module_x_x_11 = moduleX(s"$projectName.module_x_x_11", LanguageLevel.JDK_11, "11")

        // Java preview features
        // NOTE: IntelliJ API supports only 2 last preview versions of java language level (in com.intellij.pom.java.LanguageLevel)
        // When a new version of Java releases and IDEA supports it, we should update this test
        //
        // no explicit target: javac will use source level by default
        val module_8_x_x_preview  = moduleX(s"$projectName.module_8_x_x_preview", LanguageLevel.JDK_1_8, null) // no preview for Java 8
        val module_11_x_x_preview = moduleX(s"$projectName.module_11_x_x_preview", LanguageLevel.JDK_11, null) // no preview for Java 11
        val module_14_x_x_preview = moduleX(s"$projectName.module_14_x_x_preview", LanguageLevel.JDK_14, null) // no preview for Java 11
        val module_20_x_x_preview = moduleX(s"$projectName.module_20_x_x_preview", LanguageLevel.JDK_20_PREVIEW, null)

        val module_x_x_8_preview  = moduleX(s"$projectName.module_x_x_8_preview", LanguageLevel.JDK_1_8, "8")
        val module_x_x_11_preview = moduleX(s"$projectName.module_x_x_11_preview", LanguageLevel.JDK_11, "11")
        val module_x_x_14_preview = moduleX(s"$projectName.module_x_x_14_preview", LanguageLevel.JDK_14, "14")
        val module_x_x_20_preview = moduleX(s"$projectName.module_x_x_20_preview", LanguageLevel.JDK_20_PREVIEW, "20")

        modules := Seq(
          root,
          module_x_x_x,
          module_8_8_x, module_8_11_x, module_11_8_x, module_11_11_x,
          module_8_x_x, module_11_x_x, module_14_x_x, module_15_x_x,
          module_x_8_x, module_x_11_x,
          module_x_x_8, module_x_x_11,
          module_8_x_x_preview, module_11_x_x_preview, module_14_x_x_preview, module_20_x_x_preview,
          module_x_x_8_preview, module_x_x_11_preview, module_x_x_14_preview, module_x_x_20_preview,
        )
      }
    ) finally {
      inWriteAction {
        ProjectJdkTable.getInstance.removeJdk(projectSdk9)
      }
    }
  }

  //noinspection TypeAnnotation
  // SCL-16204, SCL-17597
  @nowarn("cat=deprecation")
  def testJavaLanguageLevelAndTargetByteCodeLevel_NoOptions(): Unit = {
    val projectLangaugeLevel = SbtProjectStructureImportingTest.this.projectJdkLanguageLevel
    val projectName = "java-language-level-and-target-byte-code-level-no-options"
    def doRunTest(): Unit = runTest(
      new project(projectName) {
        javacOptions := Nil
        javaLanguageLevel := projectLangaugeLevel
        javaTargetBytecodeLevel := null

        val root = new module(s"$projectName") {
          javaLanguageLevel := projectLangaugeLevel
          javaTargetBytecodeLevel := null
          javacOptions := Nil
        }
        val module1 = new module(s"$projectName.module1") {
          javaLanguageLevel := projectLangaugeLevel
          javaTargetBytecodeLevel := null
          javacOptions := Nil
        }

        modules := Seq(root, module1)
      }
    )

    doRunTest()

    // Emulate User changing the settings manually
    ExternalSystemApiUtil.executeProjectChangeAction(new DisposeAwareProjectChange(myProject) {
      override def execute(): Unit = {
        val ManuallySetTarget = "9"
        val ManuallySetSource = LanguageLevel.JDK_1_9

        setOptions(myProject, ManuallySetSource, ManuallySetTarget, Seq("-some-root-option"))

        val projectModules = myProject.modules
        projectModules.foreach(setOptions(_, ManuallySetSource, ManuallySetTarget, Seq("-some-module-option")))
      }
    })

    // Manually set settings should be rewritten if no explicit javac options provided
    doRunTest()
  }

  //noinspection TypeAnnotation
  def testJavacOptionsPerModule(): Unit = {
    val projectName = "javac-options-per-module"
    runTest(
      new project(projectName) {
        javacOptions := Nil // no storing project level options

        def moduleX(name: String, expectedJavacOptions: Seq[String]): module = new module(s"$projectName.$name") {
          javacOptions := expectedJavacOptions
        }

        // TODO: currently IDEA doesn't support more finely-grained scopes,like `in (Compile, compile)
        //  so option root_option_in_compile_compile is not included
        //  IDEA-232043, SCL-11883, SCL-17020
        val root = new module(projectName) {
          javacOptions := Seq("root_option", "root_option_in_compile")
        }

        val module1 = moduleX("module1", Seq("module_1_option"))
        val module2 = moduleX("module2", Seq("module_2_option_in_compile"))
        val module3 = moduleX("module3", Seq())

        modules := Seq(root, module1, module2, module3)
      }
    )
  }

  def testJavacSpecialOptionsForRootProject(): Unit = {
    runTest(
      new project("javac-special-options-for-root-project") {
        // no storing project level options
        javacOptions := Nil
        javaTargetBytecodeLevel := null
        javaLanguageLevel := SbtProjectStructureImportingTest.this.projectJdkLanguageLevel

        val root: module = new module("javac-special-options-for-root-project") {
          javaLanguageLevel := LanguageLevel.JDK_1_9
          javaTargetBytecodeLevel := "1.7"
          javacOptions := Seq(
            "-g:none",
            "-nowarn",
            "-deprecation",
            "-Werror"
          )
        }
        modules:= Seq(root)
      }
    )

    val compilerOptions = JavacConfiguration.getOptions(myProject, classOf[JavacConfiguration])
    val defaultCompilerOptions = new JpsJavaCompilerOptions

    assertEquals(defaultCompilerOptions.DEBUGGING_INFO, compilerOptions.DEBUGGING_INFO)
    assertEquals(defaultCompilerOptions.GENERATE_NO_WARNINGS, compilerOptions.GENERATE_NO_WARNINGS)
    assertEquals(defaultCompilerOptions.DEPRECATION, compilerOptions.DEPRECATION)
    assertEquals(defaultCompilerOptions.ADDITIONAL_OPTIONS_STRING, compilerOptions.ADDITIONAL_OPTIONS_STRING)
    assertEquals(defaultCompilerOptions.MAXIMUM_HEAP_SIZE, compilerOptions.MAXIMUM_HEAP_SIZE)
    assertEquals(defaultCompilerOptions.PREFER_TARGET_JDK_COMPILER, compilerOptions.PREFER_TARGET_JDK_COMPILER)
  }

  def testCompileOrder(): Unit = {
    runTest(
      new project("compile-order-unspecified") {
        modules := Seq(
          new module("compile-order-unspecified") {
            compileOrder := CompileOrder.Mixed
          },
          new module("compile-order-unspecified.compile-order-mixed") {
            compileOrder := CompileOrder.Mixed
          },
          new module("compile-order-unspecified.compile-order-scala-then-java") {
            compileOrder := CompileOrder.ScalaThenJava
          },
          new module("compile-order-unspecified.compile-order-java-then-scala") {
            compileOrder := CompileOrder.JavaThenScala
          }
        )
      }
    )
  }

  def testSimpleProjectWithGeneratedSources(): Unit = runTest(
    new project("SimpleProjectWithGeneratedSources") {
      modules := Seq(
        new module("SimpleProjectWithGeneratedSources") {
          sources := Seq(
            "src/main/scala",
            "target/scala-2.13/src_managed/main",
            "target/myGenerated/main",
          )
          testSources := Seq(
            "src/test/scala",
            "target/scala-2.13/src_managed/test",
            "target/myGenerated/test",
          )
          resources := Seq(
            "src/main/resources",
            "target/scala-2.13/resource_managed/main"
          )
          testResources := Seq(
            "src/test/resources",
            "target/scala-2.13/resource_managed/test"
          )
          excluded := Seq("target")
        },
        new module("SimpleProjectWithGeneratedSources.SimpleProjectWithGeneratedSources-build") {},
      )
    }
  )

  def testCustomConfigurationsWithNestedProjectDependencies(): Unit = {
    val projectName = "customConfigurationsWithNestedProjectDependencies"
    runTest(
      new project(projectName) {

        lazy val root: module = new module(projectName) {
          sbtProjectId := "root"
          moduleDependencies := Seq()
        }

        lazy val foo: module = new module(s"$projectName.foo") {
          sbtProjectId := "foo"
          moduleDependencies := Seq(
            new dependency(root) {
              isExported := false
              scope := DependencyScope.TEST
            }
          )
        }

        lazy val utils: module = new module(s"$projectName.utils") {
          sbtProjectId := "utils"
          moduleDependencies := Seq(
            new dependency(foo) {
              isExported := false
              scope := DependencyScope.COMPILE
            },
            new dependency(root) {
              isExported := false
              scope := DependencyScope.TEST
            }
          )
        }
        modules := Seq(utils, foo, root)
      }
    )
  }

  def testProjectWithModulesWithSameIdsAndNamesWithDifferentCase(): Unit = runTest(
    new project("sameIdsAndNamesWithDifferentCase") {
      modules := Seq(
        new module ("sameIdsAndNamesWithDifferentCase"),
        new module ("U_MY_MODULE_ID~2", Array("sameIdsAndNamesWithDifferentCase", "same module name")),
        new module ("U_My_Module_Id~1", Array("sameIdsAndNamesWithDifferentCase","same module name")),
        new module ("U_my_module_id", Array("sameIdsAndNamesWithDifferentCase","same module name")),
        new module ("sameIdsAndNamesWithDifferentCase.X_MY_MODULE_ID~2"),
        new module ("sameIdsAndNamesWithDifferentCase.X_My_Module_Id~1"),
        new module ("sameIdsAndNamesWithDifferentCase.X_my_module_id"),
        new module ("sameIdsAndNamesWithDifferentCase.Y_MY_MODULE_Name~2"),
        new module ("sameIdsAndNamesWithDifferentCase.Y_My_Module_Name~1"),
        new module ("sameIdsAndNamesWithDifferentCase.Y_my_module_name"),
        new module ("sameIdsAndNamesWithDifferentCase.Z_MY_MODULE_Name~2"),
        new module ("sameIdsAndNamesWithDifferentCase.Z_My_Module_Name~1"),
        new module ("sameIdsAndNamesWithDifferentCase.Z_my_module_name"),
      )
    }
  )

  //corresponds to logic described in org.jetbrains.sbt.project.SbtProjectResolver.generateUniqueModuleInternalNameForRootProject
  def testMultiBuildProjectWithSpecialCharactersInRootProjectNames(): Unit = runTest(
    new project("ro//o.t.") {
      val buildURI: URI = getTestProjectDir.getCanonicalFile.toURI

      val rootC1: module = new module("Build__1_N_ame") {
        sbtProjectId := "root"
        sbtBuildURI := buildURI.resolve("c1/")
        moduleDependencies := Seq()
      }
      val root: module = new module("ro__o_t_") {
        sbtProjectId := "root"
        sbtBuildURI := buildURI
        moduleDependencies += new dependency(rootC1) { isExported := false }
      }

      val modulesRoot: Seq[module] = Seq(
        root,
        new module("project1", Array("ro__o_t_")),
      )
      val modulesC1: Seq[module] = Seq(
        rootC1,
        new module("project1", Array("Build__1_N_ame")),
      )

      modules := modulesRoot ++ modulesC1
    }
  )

  def testSharedSourcesInsideMultiBuildProject(): Unit = {
    val projectName = "sharedSourcesInsideMultiBuildProject"
    runTest(
      new project(projectName) {
        lazy val scalaLibraries: Seq[library] = ProjectStructureTestUtils.expectedScalaLibraryWithScalaSdk("2.13.6")
        libraries := scalaLibraries

        val buildURI: URI = getTestProjectDir.getCanonicalFile.toURI

        lazy val c1: module = new module("c1") {
          contentRoots := Seq(getProjectPath + "/c1")
          sbtProjectId := "c1"
          sbtBuildURI := buildURI.resolve("c1/")
          libraryDependencies := scalaLibraries
        }

        lazy val root: module = new module(projectName) {
          contentRoots := Seq(getProjectPath)
          sbtProjectId := "sharedSourcesInsideMultiBuildProject"
          sbtBuildURI := buildURI
          libraryDependencies := scalaLibraries
          moduleDependencies += new dependency(c1) { isExported := false }
        }

        val sharedSourcesModuleInC1: module = new module("c1-sources", Array("c1")) {
          libraryDependencies := scalaLibraries
        }
        val c1Modules: Seq[module] = Seq(
          sharedSourcesModuleInC1,
          new module("foo", Array("c1")) {
            libraryDependencies := scalaLibraries
            sbtProjectId := "foo"
            sbtBuildURI := buildURI.resolve("c1/")
            moduleDependencies += new dependency(sharedSourcesModuleInC1) { isExported := true }

          },
          new module("bar", Array("c1")) {
            libraryDependencies := scalaLibraries
            sbtProjectId := "bar"
            sbtBuildURI := buildURI.resolve("c1/")
            moduleDependencies += new dependency(sharedSourcesModuleInC1) { isExported := true }
          }
        )

        modules := c1 +: root +: c1Modules
      }
    )
  }

  // SBT guarantees us that project ids inside builds are unique. In IDEA in the internal module name all "/" are replaced with "_" and it could happen that in one build
  // the name of one project would be e.g. ro/t and the other one would be ro_t and for SBT project ids uniqueness would be maintained but not for IDEA.
  // In such case we should handle it and append number suffix to one of the module name
  def testMultiBuildProjectWithTheSameProjectIdFromIDEAPerspective(): Unit = runTest(
    new project("multiBuildProjectWithTheSameProjectIdFromIDEAPerspective") {
      lazy val scalaLibraries: Seq[library] = ProjectStructureTestUtils.expectedScalaLibraryWithScalaSdk("2.13.6")
      libraries := scalaLibraries

      val buildURI: URI = getTestProjectDir.getCanonicalFile.toURI

      lazy val c1: module = new module("c1") {
        contentRoots := Seq(getProjectPath + "/c1")
        sbtProjectId := "c1"
        sbtBuildURI := buildURI.resolve("c1/")
        libraryDependencies := scalaLibraries
      }

      lazy val root: module = new module("multiBuildProjectWithTheSameProjectIdFromIDEAPerspective") {
        contentRoots := Seq(getProjectPath)
        sbtProjectId := "multiBuildProjectWithTheSameProjectIdFromIDEAPerspective"
        sbtBuildURI := buildURI
        libraryDependencies := scalaLibraries
        moduleDependencies += new dependency(c1) { isExported := false }
      }


      val c1Modules: Seq[module] = Seq(
        new module("ro_t", Array("c1")) {
          libraryDependencies := scalaLibraries
          sbtProjectId := "mod1"
          sbtBuildURI := buildURI.resolve("c1/")
        },
        new module("ro_t~1", Array("c1")) {
          libraryDependencies := scalaLibraries
          sbtProjectId := "mod2"
          sbtBuildURI := buildURI.resolve("c1/")
        }
      )

      modules := c1 +: root +: c1Modules
    }
  )

  def testProjectWithJmhPlugin(): Unit = runTest(
    new project("projectWithJmhPlugin") {
      lazy val root: module = new module("projectWithJmhPlugin")

      lazy val project1: module = new module("projectWithJmhPlugin.project1") {
        sbtProjectId := "project1"
        contentRoots := Seq(getProjectPath + "/project1")
        compileOutputPath := "target/scala-2.13/classes"
        compileTestOutputPath := "target/scala-2.13/test-classes"
      }

      lazy val project2: module = new module("projectWithJmhPlugin.project2") {
        sbtProjectId := "project2"
        contentRoots := Seq(getProjectPath + "/project2")
        compileOutputPath := "target/scala-2.13/classes"
        compileTestOutputPath := "target/scala-2.13/test-classes"
      }

      modules := root :: project1 :: project2 :: Nil
    }
  )
}