object Constants {
    const val kotlinVersion = "1.5.10"
}

plugins {
    kotlin("multiplatform").version("1.5.10")
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
              // implementation(libs.bignum)
              implementation("com.ionspin.kotlin:bignum:0.3.1")
              implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.5.0")
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
