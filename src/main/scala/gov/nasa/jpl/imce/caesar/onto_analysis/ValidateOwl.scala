/*
 * Copyright 2019 Jet Propulsion Laboratory, California Institute of Technology
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package gov.nasa.jpl.imce.caesar.onto_analysis

import enumeratum._
import java.io.File
import java.nio.file.Files
import java.util.Properties

import com.clarkparsia.owlapi.explanation.GlassBoxExplanation
import com.clarkparsia.pellet.owlapiv3.PelletReasonerFactory
import com.typesafe.scalalogging.Logger
import io.circe.{yaml, Json, JsonObject, Printer}
import org.mindswap.pellet.PelletOptions
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.formats.FunctionalSyntaxDocumentFormat
import org.semanticweb.owlapi.io.StringDocumentTarget
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.util.{PriorityCollection, SimpleIRIMapper, SimpleRenderer}
import scopt.{OParser, OParserBuilder}
import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl

import scala.collection.immutable.{Map, Set}
import scala.jdk.javaapi.CollectionConverters
import scala.util.control.Exception

object ValidateOwl {

  val logger: Logger = Logger[ValidateOwl.type]

  sealed trait Test extends EnumEntry

  object Test extends Enum[Test] {
    val values = findValues

    case object Consistency extends Test

    case object Satisfiability extends Test

  }

  case class Config(
    tests: Seq[Test] = Seq(Test.Consistency),
    indent: Int = 2,
    indicateStatus: Boolean = false,
    id: String = "",
    useUNA: Boolean = false,
    pruneExplanations: Boolean = false,
    pruneIncrementally: Boolean = false,
    locationMapping: JsonObject = JsonObject.empty,
    iris: Seq[IRI] = Seq.empty)

  implicit val iriRead: scopt.Read[IRI] =
    scopt.Read.reads(IRI.create)

  val builder: OParserBuilder[Config] = OParser.builder[Config]
  val parser1: OParser[Unit, Config] = {
    import builder._
    OParser.sequence(
      programName("validate-owl"),
      opt[Boolean]("satisfiability")
        .action(
          (x, c) =>
            if (x)
              c.copy(tests = c.tests :+ Test.Satisfiability)
            else
              c
        )
        .text("validate satisfiability"),
      opt[Int]("indent")
        .action((x, c) => c.copy(indent = x))
        .text("indent output"),
      opt[String]("id")
        .required()
        .action((x, c) => c.copy(id = x))
        .text("identifier"),
      opt[Boolean]("use-una")
        .action((x, c) => c.copy(useUNA = x))
        .text("use Pellet unique name assumption"),
      opt[Boolean]("prune-explanations")
        .action((x, c) => c.copy(pruneExplanations = x))
        .text("prune explanations"),
      opt[Boolean]("prune-incrementally")
        .action((x, c) => c.copy(pruneIncrementally = x))
        .text("prune incrementally"),
      opt[File]("location-mapping")
        .action(
          (f, c) =>
            try {
              val reader = Files.newBufferedReader(f.toPath)
              yaml.parser.parse(reader) match {
                case Right(json: Json) =>
                  json.asObject match {
                    case Some(mapping: JsonObject) =>
                      c.copy(locationMapping = mapping)
                    case _ =>
                      c
                  }
                case Left(_) =>
                  c
              }
            } catch {
              case _: Throwable =>
                c
            }
        )
        .validate(
          f =>
            try {
              val reader = Files.newBufferedReader(f.toPath)
              yaml.parser.parse(reader) match {
                case Right(json: Json) =>
                  json.asObject match {
                    case Some(_: JsonObject) =>
                      success
                    case _ =>
                      failure(s"No location mapping Yaml found in file: $f")
                  }
                case Left(error) =>
                  failure(error.message)
              }
            } catch {
              case t: Throwable =>
                failure(t.getMessage)
            }
        )
        .text("location mapping file"),
      arg[IRI]("<IRI>...")
        .unbounded()
        .action((x, c) => c.copy(iris = c.iris :+ x))
        .text("ontology IRIs")
    )
  }

  def main(
    args: Array[String]
  ): Unit = {

    OParser.parse(parser1, args, Config()) match {
      case Some(config) =>
        Exception
          .nonFatalCatch[OWLException]
          .withApply(t => {
            System.err.println(t.getMessage)
            t.printStackTrace(System.err)
            System.exit(255)
          })
          .apply {
            validate(config)
          }
      case _ =>
        // arguments are bad, error message will have been printed.
        System.exit(255)
    }
  }

  def validate(
    config: Config
  ): Unit = {

    implicit val manager: OWLOntologyManager = OWLManager.createOWLOntologyManager

    val mappers: PriorityCollection[OWLOntologyIRIMapper] = manager.getIRIMappers
    val locMappers = config.locationMapping.toMap.flatMap {
      case (iri, value) =>
        value.asString match {
          case Some(file) =>
            val ontologyIRI = IRI.create(iri)
            val documentIRI = IRI.create(file)
            val mapper: OWLOntologyIRIMapper = new SimpleIRIMapper(ontologyIRI, documentIRI)
            Some(mapper)
          case _ =>
            None
        }
    }

    val jlocMappers = CollectionConverters.asJava(locMappers)
    mappers.add(jlocMappers)

    val functional_syntax_format: FunctionalSyntaxDocumentFormat = new FunctionalSyntaxDocumentFormat()

    // Create owl:Thing.

    val owl_thing: OWLClass = (new OWLDataFactoryImpl).getOWLThing

    // Create collection ontology.

    val collection_ontology: OWLOntology = manager.createOntology()

    config.iris.foreach {
      iri =>
        // Load ontology.

        logger.debug(s"load ontology $iri")
        val this_ontology: OWLOntology = manager.loadOntology(iri)

        // Add axioms from this ontology and its imports to collection ontology.

        manager.addAxioms(collection_ontology, this_ontology.getAxioms())
        manager.getImportsClosure(this_ontology).forEach {
          o: OWLOntology =>
            manager.addAxioms(collection_ontology, o.getAxioms())
        }

        // Get ontology format.

        logger.debug("get ontology format")
        Option.apply(manager.getOntologyFormat(this_ontology)) match {
          case Some(format: PrefixManager) =>
            // Copy ontology prefixes to output format.

            logger.debug("copy ontology format to output format")
            functional_syntax_format.copyPrefixesFrom(format)

          case Some(_) =>
            logger.debug("no prefixes to copy to output format")

          case None =>
            logger.error("no ontology format!")
          // Is this a fatal error?
        }
    }

    // Get axiom counts.

    val os: Set[OWLOntology] = CollectionConverters.asScala(manager.getOntologies()).toSet
    val total: Int = (os - collection_ontology).map {
      o: OWLOntology =>
        val count: Int = o.getLogicalAxiomCount
        val i = o.getOntologyID.getOntologyIRI.orNull()
        logger.debug(s"loaded $count logical axioms from $i")
        count
    }.sum

    logger.info(s"total $total logical axioms")
    logger.info(s"total ${collection_ontology.getLogicalAxiomCount} collected logical axioms")

    // Create renderer for unsatistiable class names.

    logger.debug("create renderer")
    val renderer = new SimpleRenderer
    renderer.setPrefixesFromOntologyFormat(collection_ontology, manager, false)

    // Create Pellet reasoner.

    logger.debug("create pellet reasoner factory")
    val reasoner_factory = new PelletReasonerFactory
    logger.debug("create pellet reasoner")
    val reasoner = reasoner_factory.createReasoner(collection_ontology)

    // Enable Unique Name Assumption, Prune Explanations and Prune Incrementally if specified.

    val properties = PelletOptions.setOptions(new Properties)
    properties.setProperty("USE_UNIQUE_NAME_ASSUMPTION", config.useUNA.toString)
    properties.setProperty("PRUNE_EXPLANATIONS", config.pruneExplanations.toString)
    properties.setProperty("PRUNE_INCREMENTALLY", config.pruneIncrementally.toString)
    PelletOptions.setOptions(properties)

    val success = new scala.collection.mutable.HashMap[Test, Map[String, Json]]
    val explanation = new scala.collection.mutable.HashMap[Test, Map[String, Json]]

    // Create GlassBoxExplanation.

    logger.debug("create glass box explanation")
    val gb_explanation = new GlassBoxExplanation(reasoner)

    // Run tests.
    config.tests.foldLeft[Boolean](true) {
      case (true, test) =>
        logger.info(s"test $test")

        test match {
          case Test.Consistency =>
            // Check consistency.

            val consistent = reasoner.isConsistent
            success += test -> Map(config.id -> Json.fromBoolean(consistent))
            logger.info(if (consistent) "consistent" else "inconsistent")

            if (!consistent) {
              explanation += test -> Map(config.id -> explain_class(owl_thing, gb_explanation, manager, functional_syntax_format))
            }

            consistent

          case Test.Satisfiability =>
            val satSuccesses = new scala.collection.mutable.HashMap[String, Json]
            val satExplanations = new scala.collection.mutable.HashMap[String, Json]

            // Check satisfiability.

            val all_classes: Set[OWLClass] = CollectionConverters.asScala(collection_ontology.getClassesInSignature(Imports.INCLUDED)).toSet
            val n_class = all_classes.size
            logger.info(s"$n_class total classes")
            all_classes.zipWithIndex.foreach {
              case (klass, i) =>
                val klass_name = renderer.render(klass)
                val case_name = s"${config.id} $klass_name"
                logger.debug(s"$klass_name (${1 + i} of $n_class)")
                val t_start = System.currentTimeMillis()
                val satisfiable = reasoner.isSatisfiable(klass)
                val t_end = System.currentTimeMillis()
                val t_delta = math.min((t_end - t_start) / 1000, 1)
                logger.debug(s"$klass_name ${if (satisfiable) "" else "un"}satisfiable ($t_delta s")
                satSuccesses += case_name -> Json.fromBoolean(satisfiable)
                if (!satisfiable) {
                  satExplanations += case_name -> explain_class(klass, gb_explanation, manager, functional_syntax_format)
                }
            }

            success += test -> satSuccesses.toMap
            explanation += test -> satExplanations.toMap

            logger.info(s"${satExplanations.size} unsatisfiable classes")

            satExplanations.isEmpty
        }

      case (false, _) =>
        false
    }

    // Result
    val json: Json = Json.fromJsonObject(
      JsonObject(
        "testsuites" -> Json.fromValues(
          config.tests.flatMap {
            test =>
              success.get(test) match {
                case Some(successes: Map[String, Json]) =>
                  val explanations: Map[String, Json] = explanation.getOrElse(test, Map.empty)
                  val testcases: Seq[Json] = successes.map {
                    case (name, result) =>
                      explanations.get(name) match {
                        case Some(failure) =>
                          Json.fromJsonObject(
                            JsonObject(
                              "testcase" -> Json.fromString(name),
                              "failure" -> failure
                            )
                          )
                        case None =>
                          Json.fromJsonObject(JsonObject("testcase" -> Json.fromString(name)))
                      }
                  }.toSeq
                  val testsuite: Json = Json.fromJsonObject(
                    JsonObject(
                      "testsuite" -> Json.fromString(test.toString),
                      "testcases" -> Json.fromValues(testcases)
                    )
                  )
                  Some(testsuite)
                case _ =>
                  None
              }
          }
        )
      )
    )

    System.out.println(Printer.spaces2.print(json))
  }

  def explain_class(
    klass: OWLClass,
    explanation: GlassBoxExplanation,
    manager: OWLOntologyManager,
    format: FunctionalSyntaxDocumentFormat
  ): Json = {
    Option.apply(explanation.getExplanation(klass)) match {
      case Some(axioms) =>
        val ontology: OWLOntology = manager.createOntology(axioms)
        val target = new StringDocumentTarget
        manager.saveOntology(ontology, format, target)
        Json.fromString(target.toString)

      case None =>
        throw new IllegalArgumentException(s"Couldn't get an explanation for: $klass")
    }
  }
}
