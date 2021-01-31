plugins {
    kotlin("multiplatform") version "1.4.21"
}

repositories {
    mavenCentral()
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
              implementation("com.ionspin.kotlin:bignum:0.2.3")
          }
      }
    }
}

tasks.withType<Wrapper> {
  gradleVersion = "6.8.1"
  distributionType = Wrapper.DistributionType.BIN
}
