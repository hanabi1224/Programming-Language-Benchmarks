object Constants {
    const val kotlinVersion = "1.5.21"
}

plugins {
    val kotlinVersion = "1.5.21"
    kotlin("multiplatform").version(kotlinVersion)
    kotlin("plugin.serialization").version(kotlinVersion)
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
