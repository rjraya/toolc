package toolc

import utils._
import java.io.File

import lexer._
import ast._
import eval._
import analyzer._
import abstract_inter._
import code._

object Main {

  def processOptions(args: Array[String]): Context = {
    val reporter = new Reporter()
    var files: List[File] = Nil
    var outDir: Option[File] = None

    def rec(args: List[String]): Unit = args match {
      case "-d" :: dir :: xs =>
        outDir = Some(new File(dir))
        rec(xs)

      case f :: xs =>
        files  ::= new File(f)
        rec(xs)

      case _ =>
    }

    rec(args.toList)

    if (files.size != 1) {
      reporter.fatal("Exactly one file expected, "+files.size+" file(s) given.")
    }

    Context(reporter = reporter, files = files, outDir = outDir)
  }


  def main(args: Array[String]) {
    val ctx = processOptions(args)

    val pipeline = Lexer andThen
                   Parser andThen
                   NameAnalysis andThen
                   TypeChecking andThen
                   StaticAnalysis andThen
                   UNTAC_CodeGeneration

    pipeline.run(ctx)(ctx.files.head)

    ctx.reporter.terminateIfErrors
  }
}
