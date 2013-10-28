# DrScala

Hi,

I'm DrScala, my goal is to take care of your code.

## Configurations

## Setup in SBT 

    autoCompilerPlugins := true

    addCompilerPlugin("com.github.aloiscochard" %% "drscala" % "0.1.0" classifier "assembly")

    scalacOptions ++= Seq(
      "debug",
      "warn",
      "gh.user=aloiscochard",
      "gh.password=42",
      "gh.repository.owner=aloiscochard",
      "gh.repository.name=drscala"
    ).map("-P:drscala:" + _)

## Code reviewing

The compiler plugin will read the env variables `DRSCALA_PR` or `ghprbPullId` as well as the system property `drscala.pr`,
to find the pull request ID that should be automatically reviewed.

## Jenkins - GitHub Pull Request Builder

DrScala can be easily integrated with the jenkins plugin that automate building of pull request.
The plugin will automatically detect the pull request to review using the env variable `ghprbPullId'.

