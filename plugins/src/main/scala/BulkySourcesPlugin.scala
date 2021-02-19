import sbt._
import Keys._

object BulkySourcesPlugin extends AutoPlugin {
  lazy val bulkyThresholdInLines =
    settingKey[Int]("Number of lines threshold")
  lazy val bulkySources = taskKey[Seq[(Int, File)]](
    "Sequence of src/tst code files with valid number of lines"
  )

  def result(sourceFilesSources: Seq[File], threshold: Int): Seq[(Int, File)] =
    sourceFilesSources
      .map(file => (sbt.IO.readLines(file).length, file))
      .filter { case (lineCount, _) => lineCount >= threshold }

  override val projectSettings: Seq[Setting[_]] = Seq(
    bulkyThresholdInLines := 100,
    bulkySources := result(
      (Compile / sources).value,
      bulkyThresholdInLines.value
    ),
    (Test / bulkySources) := result(
      (Test / sources).value,
      bulkyThresholdInLines.value
    )
  )
}
