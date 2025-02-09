import com.github.benmanes.gradle.versions.updates.DependencyUpdatesTask
import org.jetbrains.kotlin.config.KotlinCompilerVersion
import org.jetbrains.kotlin.gradle.plugin.mpp.KotlinNativeTarget

plugins {
  val kotlinVersion = "2.1.10"
  kotlin("multiplatform").version(kotlinVersion)
  kotlin("plugin.serialization").version(kotlinVersion)
  id("com.github.ben-manes.versions").version("0.52.0")
}

repositories {
  google()
  mavenCentral()
  gradlePluginPortal()
}

kotlin {
  linuxX64("native") { binaries { executable("_app") } }

  sourceSets {
    val commonMain by getting {
      dependencies {
        implementation(libs.bignum)
        implementation(libs.kbignum)
        // implementation("com.ionspin.kotlin:bignum:0.3.1")
        implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.10.1")
        implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:1.8.0")
      }
    }
  }
}

// https://github.com/JetBrains/kotlin/blob/master/kotlin-native/NEW_MM.md
// To verify:
// // main.kt
// @kotlin.ExperimentalStdlibApi
// fun main(args: Array<String>) {
//   println(kotlin.native.isExperimentalMM())
// }
//
kotlin.targets.withType(KotlinNativeTarget::class.java) {
  binaries.all {
     freeCompilerArgs += "opt"
  }
}

tasks.register("version") {
  doLast {
    val jreVersion = System.getProperty("java.runtime.version")
    println("kotlinc-native ${KotlinCompilerVersion.VERSION} (JRE $jreVersion)")
  }
}

tasks.register("du") { dependsOn("dependencyUpdates") }

tasks.named<DependencyUpdatesTask>("dependencyUpdates") {
  rejectVersionIf { isNonStable(candidate.version) && !isNonStable(currentVersion) }
  // optional parameters
  checkForGradleUpdate = true
  outputFormatter = "json"
  outputDir = "build/dependencyUpdates"
  reportfileName = "report"
}

fun isNonStable(version: String): Boolean {
  val stableKeyword = listOf("RELEASE", "FINAL", "GA").any { version.toUpperCase().contains(it) }
  val regex = "^[0-9,.v-]+(-r)?$".toRegex()
  val isStable = stableKeyword || regex.matches(version)
  return isStable.not()
}
