import com.github.benmanes.gradle.versions.updates.DependencyUpdatesTask
import org.jetbrains.kotlin.config.KotlinCompilerVersion

plugins {
  val kotlinVersion = "1.6.10"
  kotlin("multiplatform").version(kotlinVersion)
  kotlin("plugin.serialization").version(kotlinVersion)
  id("com.github.ben-manes.versions").version("0.41.0")
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
        implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.6.0")
        implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.2")
      }
    }
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
