package io.heta.tap.data

object DocTypes {
  type Term = String
  type Lemma = String
  type Stem = String
  type POStag = String
  type NERtag = String
  type Terms = Vector[Term]
  type Lemmas = Vector[Lemma]
  type Stems = Vector[Stem]
  type POStags = Vector[POStag]
  type NERtags = Vector[NERtag]
}
