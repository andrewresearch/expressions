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

package io.heta.tap.analysis.affectlexicon

import io.heta.tap.analysis.Lexicons.Lexicon
import io.heta.tap.data.DocTypes.{Lemmas, Terms}
import io.heta.tap.data.doc.expression.affect.{Affect, AffectExpression}

/**
  * Created by quanie on 13/11/17.
  */


/**
  * Performs the analysis which shows if an input terms are positive or negative
  */

class AffectLexiconManager(allAffectTerms:Vector[Affect]) {

  //val allAffectTerms:AffectLexicon = load("/affect-lexicon.json")
  //logger.info(s"Affect lexicon loaded with ${allAffectTerms.size} words.")
  private val subsets = getSubSets()
  val mostNegativeTerms:Lexicon = subsets._1.map(_.word)
  val mostPositiveTerms:Lexicon = subsets._2.map(_.word)
  val minValence:Double = subsets._3
  val maxValence:Double = subsets._4


  /**
    * Initialize
    *
    * @return with Boolean expression
    */
  def init:Boolean = {
    allAffectTerms.size > 0
  }

  /**
    * A GET request operation using a bucket URI lists information about the objects in the bucket.
    *
    * @param lemmas
   *  @param terms
    * @return A vector containing [AffectExpression]
    */
  //def getAllMatchingTerms(tokens:Vector[Token]):Vector[AffectExpression] = {
  def getAllMatchingTerms(terms:Terms,lemmas:Lemmas):Vector[AffectExpression] = {
    val indexedLemmas = lemmas.zipWithIndex
    val affectTerms = allAffectTerms.filter(a => lemmas.contains(a.word)).map(aff => aff.word -> aff).toMap
    indexedLemmas.map { case (l,i) =>
      val affect = affectTerms.getOrElse(l,Affect(l,0,0,0))
      AffectExpression(terms(i),i,i,affect.valence,affect.arousal,affect.dominance)
    }
  }

  /**
    * Checks if the terms are positive.
    *
    * @param terms Word or expression
    * @return A vector containing [AffectExpression]
    */
  private def getPositive(terms: Terms): Vector[AffectExpression] = {
    val pos = terms.filter(l => mostPositiveTerms.contains(l))
    pos.map(w => AffectExpression(w,-1,-1))
  }

  /**
    * Checks if the terms are negative.
    *
    * @param terms Word or expression
    * @return A vector containing [AffectExpression]
    */
  private def getNegative(terms: Terms): Vector[AffectExpression] = {
    val neg = terms.filter(l => mostNegativeTerms.contains(l))
    neg.map(w => AffectExpression(w,-1,-1))
  }

  /**
    * Check if the terms are positive or negative.
    *
    * @param terms
    * @return
    */
  //def getAffectTerms(tokens:Vector[Token]):Vector[AffectExpression] = {
    //val terms = tokens.filterNot(_.isPunctuation).map(_.term.toLowerCase)
  def getAffectTerms(terms:Terms):Vector[AffectExpression] = {
    val lowerTerms = terms.map(_.toLowerCase)
    val posWords = getPositive(lowerTerms)
    val negWords = getNegative(lowerTerms)

    posWords ++ negWords
  }

  /**
    * TODO
    *
    * @param threshold the magnitude that must be exceeded for a certain result
    * @return
    */
  def mostEmotional(threshold:Double=3.5):Vector[Affect] = allAffectTerms.filter(_.arousal> threshold).sortBy(_.valence)

  /**
    * TODO
    *
    * @param ratio
    * @param threshold the magnitude that must be exceeded for a certain result
    * @return
    */
  def getSubSets(ratio:Double = 0.25, threshold:Double = 3.0):(Vector[Affect],Vector[Affect],Double,Double) = {
    val me = mostEmotional(threshold)
    val valence = me.map(_.valence)
    val size = (ratio * me.size).toInt
    (me.take(size),me.takeRight(size),valence.min,valence.max)
  }

}
