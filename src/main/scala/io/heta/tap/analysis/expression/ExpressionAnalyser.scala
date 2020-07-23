/*
 * Copyright (c) 2016-2018 original author or authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * You may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under
 * the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied. See the License for the specific
 * language governing permissions and limitations under the License.
 *
 */

package io.heta.tap.analysis.expression

import io.heta.tap.analysis.Lexicons
import io.heta.tap.analysis.affectlexicon.AffectLexiconManager
import io.heta.tap.data.DocTypes.{Lemmas, POStags, Terms}
import io.heta.tap.data.doc.expression.{EpistemicExpression, ModalExpression}
import io.heta.tap.data.doc.expression.affect.{Affect, AffectExpression}
import io.heta.tap.data.doc.expression.reflect.ReflectExpressions

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
/**
  * Created by andrew@andrewresearch.net on 16/10/17.
  */

/**
  *
  */
class ExpressionAnalyser(allAffectTerms: Vector[Affect], reflectiveExpressionAnalyser: String => ReflectExpressions) {

  //  /**
  //    * Load a file
  //    *
  //    * @param filename
  //    * @return
  //    */
  //  def load(filename:String):Vector[Affect] = {
  //    //import play.api.libs.json._
  //    lazy val stream : InputStream = getClass.getResourceAsStream(filename)
  //    lazy val src = scala.io.Source.fromInputStream( stream )
  //    implicit val affectReads = Json.reads[Affect]
  //    val jsonString: JsValue = Json.parse(src.getLines().mkString)
  //    Json.fromJson[List[Affect]](jsonString).getOrElse(List()).toVector
  //  }

  val affectLexiconManager = new AffectLexiconManager(allAffectTerms)

  def reflective(text:String):ReflectExpressions = reflectiveExpressionAnalyser(text)  //ReflectiveExpressionPipeline.process(text)

  /** Get all matching terms */
  def affective(terms: Terms,lemmas: Lemmas):Vector[AffectExpression] = {
    affectLexiconManager.getAllMatchingTerms(terms,lemmas) //Multi-dimensional affect
  }

  /** Get affect terms, if positive or negative */
  def affect(terms: Terms): Vector[AffectExpression] = {
    affectLexiconManager.getAffectTerms(terms)
  }


  /** Epistemic Expression */
  def epistemic(terms: Terms,lemmas: Lemmas,posTags: POStags):Vector[EpistemicExpression] = {
    val indexedLemmas = lemmas.zipWithIndex
    val indexedPosTags = posTags.zipWithIndex
    //Get the indexes of any epistemic verbs
    val epIdx = indexedLemmas.filter( li => Lexicons.epistemicVerbLemmas.contains(li._1)).map(_._2)
    //Get the indexes of any personal pronouns
    val prpIdx = indexedPosTags.filter{ ti => ti._1.contains("PRP")}.map(_._2)
    //For each verb, check if there is pronoun index prior within 4 steps
    val pairs:Vector[(Option[Int],Int)] = epIdx.map(ei => (prpIdx.find(pi => (ei - pi) > 0 && (ei - pi) < 4),ei))
    pairs.map(p => EpistemicExpression(terms.slice(p._1.getOrElse(p._2),p._2+1).mkString(" "), p._1.getOrElse(p._2), p._2))
  }


  /** Modal Expression */
  def modal(terms: Terms,posTags: POStags):Vector[ModalExpression] = {
    val indexedPosTags = posTags.zipWithIndex
    //Get the indexes of any modals
    val modIdx = indexedPosTags.filter{ ti => ti._1.contains("MD")}.map(_._2)
    //Get the indexes of any personal pronouns
    val prpIdx = indexedPosTags.filter{ ti => ti._1.contains("PRP")}.map(_._2)
    //For each verb, check if there is pronoun index prior within 4 steps
    val pairs:Vector[(Option[Int],Int)] = modIdx.map(mi => (prpIdx.find(pi => (mi - pi) > 0 && (mi - pi) < 4),mi))
    pairs.map(p => ModalExpression(terms.slice(p._1.getOrElse(p._2),p._2+1).mkString(" "), p._1.getOrElse(p._2), p._2))
  }
}
