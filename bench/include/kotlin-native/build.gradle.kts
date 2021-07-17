import com.github.benmanes.gradle.versions.updates.DependencyUpdatesTask

object Constants {
    const val kotlinVersion = "1.5.21"
}

plugins {
    val kotlinVersion = "1.5.21"
    kotlin("multiplatform").version(kotlinVersion)
    kotlin("plugin.serialization").version(kotlinVersion)
    id("com.github.ben-manes.versions").version("0.39.0")
}

repositories {
    google()
    mavenCentral()
    gradlePluginPortal()
}

kotlin {
  linuxX64("native") {
    binaries {
      executable("_app")
    }
  }

  sourceSets {
      val commonMain by getting {
          dependencies {
              implementation(libs.bignum)
              // implementation("com.ionspin.kotlin:bignum:0.3.1")
              implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.5.1")
              implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:1.2.2")
          }
      }
    }
}

tasks.register("kotlinVersion") {
  doLast {
    // $HOME/.konan/kotlin-native-prebuilt-linux-${Constants.kotlinVersion}/bin/kotlinc-native -version
    println("Kotlin/Native ${Constants.kotlinVersion}")
  }
}

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
