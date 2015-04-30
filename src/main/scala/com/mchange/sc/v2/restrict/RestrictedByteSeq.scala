package com.mchange.sc.v2.restrict;

import scala.collection.immutable;

object RestrictedByteSeq {
  type ShieldType = AnyVal with RestrictedType.Element[immutable.Seq[Byte]];

  // 2.11 compiler inadequacy, does not recognize ShieldType as an extendable type, alas.

  object AnyByteSeq extends RestrictedByteSeq[AnyByteSeq] {
    override def contains( value : immutable.Seq[Byte] ) : Boolean = true;
    override def create( value : immutable.Seq[Byte] ) = new AnyByteSeq( value ); 
  }
  class AnyByteSeq private ( val value : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];

  abstract class LimitedLength[SHIELD <: ShieldType]( val MaxLengthInclusive : Int ) extends RestrictedByteSeq[SHIELD] {
    def contains( seq : immutable.Seq[Byte] ) : Boolean = seq.length <= MaxLengthInclusive;

    override def mathRep : String = s"{ b | b \u2208 immutable.Seq[Byte] \u2227 b.length \u2264 ${MaxLengthInclusive} }"
  }

  abstract class ExactLength[SHIELD <: ShieldType]( val RequiredLength : Int ) extends RestrictedByteSeq[SHIELD] {
    def contains( seq : immutable.Seq[Byte] ) : Boolean = seq.length == RequiredLength;

    override def mathRep : String = s"{ b | b \u2208 immutable.Seq[Byte] \u2227 b.length = ${RequiredLength} }"
  }
}
trait RestrictedByteSeq[SHIELD <: RestrictedByteSeq.ShieldType] extends RestrictedType[CommonConversions.ToByteSeq.type,immutable.Seq[Byte],SHIELD];
