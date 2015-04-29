package com.mchange.sc.v2.restrict;

object RestrictedBigInt {
  object AnyBigInt extends RestrictedBigInt[AnyBigInt] {
    override def contains( value : BigInt ) : Boolean = true;
    override def create( value : BigInt ) = new AnyBigInt( value ); 
  }
  class AnyBigInt private ( val value : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];

  object UnsignedBigInt extends Unsigned[UnsignedBigInt] {
    override def create( value : BigInt ) = new UnsignedBigInt( value ); 
  }
  class UnsignedBigInt private ( val value : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];

  abstract class MinUntil[SHIELD <: AnyVal]( val MinValueInclusive : BigInt, val MaxValueExclusive : BigInt ) extends RestrictedBigInt[SHIELD] with RestrictedType.MinUntil {
    require( MaxValueExclusive > MinValueInclusive );
    override def contains( value : BigInt ) : Boolean = value >= MinValueInclusive && value < MaxValueExclusive;
  }
  abstract class ZeroUntil[SHIELD <: AnyVal]( max : BigInt ) extends RestrictedBigInt.MinUntil[SHIELD](0, max );
  abstract class UnsignedWithBitLength[SHIELD <: AnyVal]( bitLength : Int ) extends RestrictedBigInt.ZeroUntil[SHIELD]( AnyBigInt(1 << bitLength).value );
  abstract class UnsignedWithByteLength[SHIELD <: AnyVal]( byteLength : Int ) extends UnsignedWithBitLength[SHIELD]( byteLength * 8 );
  abstract class Unsigned[SHIELD <: AnyVal] extends RestrictedBigInt[SHIELD] {
    override def contains( value : BigInt ) : Boolean = value >= 0;
    override def mathRep = "[0,\u221E)";
  }
}
trait RestrictedBigInt[SHIELD <: AnyVal] extends RestrictedType[CommonConversions.IntegralToBigInt.type,BigInt,SHIELD];
