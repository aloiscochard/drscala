# DrScala

Hi,

I'm DrScala, my goal is to take care of your code.

# Setup in SBT 

    autoCompilerPlugins := true

    addCompilerPlugin("com.github.aloiscochard" %% "drscala" % "0.1.0" classifier "assembly")

    scalacOptions ++= Seq(
      "warn",
      "gh.user=aloiscochard",
      "gh.password=42",
      "gh.repository.owner=aloiscochard",
      "gh.repository.name=drscala"
    ).map("-P:drscala:" + _)

## Jenkins - GitHub Pull Request Builder

DrScala can be easily integrated with the jenkins plugin that automate building of pull request.

-Ddrscala.pr=${ghprbPullId}
