package com.mchange.sc.v2.restrict;

import scala.collection.immutable;

import java.nio.charset.Charset;

object RestrictedString {
  object Restriction {
    def apply( charset : Charset ) : Restriction = new Restriction( charset.displayName, str => charset.newEncoder.canEncode( str ) )
  }
  final case class Restriction( okRep : String, ok : String => Boolean ) {
    override def toString = if ( okRep == null ) "" else s"\u2227 ${okRep} "
  }

  private val NoRestriction = Restriction( null, _ => true ); //so sue me

  type Shield = AnyVal with RestrictedType.Element[String];

  abstract class NamedRestriction[SHIELD <: Shield]( restriction : Restriction ) extends RestrictedString[SHIELD] {
    def this( charset : Charset ) = this( Restriction( charset ) )

    def contains( string : String ) : Boolean = restriction.ok( string )

    override def mathRep : String = s"{ s | s \u2208 String ${restriction} }"
  }

  abstract class LimitedLength[SHIELD <: Shield]( val MaxLengthInclusive : Int, extraRestriction : Restriction = NoRestriction ) extends RestrictedString[SHIELD] {
    def this( MaxLengthInclusive : Int, charset : Charset ) = this( MaxLengthInclusive, Restriction( charset ) )

    def contains( string : String ) : Boolean = string.length <= MaxLengthInclusive && extraRestriction.ok( string )

    override def mathRep : String = s"{ s | s \u2208 String ${extraRestriction}\u2227 s.length \u2264 ${MaxLengthInclusive} }"
  }

  abstract class ExactLength[SHIELD <: Shield]( val RequiredLength : Int, extraRestriction : Restriction = NoRestriction ) extends RestrictedString[SHIELD] {
    def this( RequiredLength : Int, charset : Charset ) = this( RequiredLength, Restriction( charset ) )

    def contains( string : String ) : Boolean = string.length == RequiredLength && extraRestriction.ok( string )

    override def mathRep : String = s"{ s | s \u2208 String ${extraRestriction}\u2227 s.length = ${RequiredLength} }"
  }
}
trait RestrictedString[SHIELD <: RestrictedString.Shield] extends RestrictedType[CommonConversions.ToString.type,String,SHIELD]

