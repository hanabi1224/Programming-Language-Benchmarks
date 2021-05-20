object Constants {
    const val kotlinVersion = "1.5.0"
}

plugins {
    kotlin("jvm").version("1.5.0")
    id("com.github.johnrengelman.shadow").version("6.1.0")
    java
    application
}

repositories {
    google()
    mavenCentral()    
    gradlePluginPortal()
}

application {
    // Define the main class for the application.
    mainClassName = "MainKt"
}

dependencies {
    implementation("org.jetbrains.kotlin:kotlin-stdlib")
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.5.0")
}

tasks.register("kotlinVersion") {
  doLast {
    // $HOME/.konan/kotlin-native-prebuilt-linux-${Constants.kotlinVersion}/bin/kotlinc-native -version
    println("Kotlinc ${Constants.kotlinVersion}")
  }
}
