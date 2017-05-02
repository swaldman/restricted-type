/*
 * Distributed as part of restricted-type v0.0.1
 *
 * Copyright (C) 2015 Machinery For Change, Inc.
 *
 * Author: Steve Waldman <swaldman@mchange.com>
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of EITHER:
 *
 *     1) The GNU Lesser General Public License (LGPL), version 2.1, as 
 *        published by the Free Software Foundation
 *
 * OR
 *
 *     2) The Eclipse Public License (EPL), version 1.0
 *
 * You may choose which license to accept if you wish to redistribute
 * or modify this work. You may offer derivatives of this work
 * under the license you have chosen, or you may provide the same
 * choice of license which you have been offered here.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * You should have received copies of both LGPL v2.1 and EPL v1.0
 * along with this software; see the files LICENSE-EPL and LICENSE-LGPL.
 * If not, the text of these licenses are currently available at
 *
 * LGPL v2.1: http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html
 *  EPL v1.0: http://www.eclipse.org/org/documents/epl-v10.php 
 * 
 */

package com.mchange.sc.v2.restrict;

object RestrictedInt {
  type Shield = AnyVal with RestrictedType.Element[Int];

  // 2.11 compiler inadequacy, does not recognize Shield as an extendable type, alas.

  object AnyInt extends RestrictedInt[AnyInt] {
    override def contains( value : Int ) : Boolean = true;
    override protected def create( value : Int ) = new AnyInt( value );
  }
  class AnyInt private ( val widen : Int ) extends AnyVal with RestrictedType.Element[Int];

  object UnsignedInt extends Unsigned[UnsignedInt] {
    override protected def create( value : Int ) = new UnsignedInt( value );
  }
  class UnsignedInt private ( val widen : Int ) extends AnyVal with RestrictedType.Element[Int];

  abstract class MinUntil[SHIELD <: Shield]( val MinValueInclusive : Int, val MaxValueExclusive : Int ) 
      extends RestrictedInt[SHIELD] with RestrictedType.MinUntil {
    require( MaxValueExclusive > MinValueInclusive );
    override def contains( value : Int ) : Boolean = value >= MinValueInclusive && value < MaxValueExclusive;
  }
  abstract class ZeroUntil[SHIELD <: Shield]( max : Int ) extends RestrictedInt.MinUntil[SHIELD](0, max );
  abstract class UnsignedWithBitLength[SHIELD <: Shield]( bitLength : Int ) extends RestrictedInt.ZeroUntil[SHIELD]( AnyInt(ONE << bitLength).widen );
  abstract class UnsignedWithByteLength[SHIELD <: Shield]( byteLength : Int ) extends UnsignedWithBitLength[SHIELD]( byteLength * 8 );
  abstract class Unsigned[SHIELD <: Shield] extends ZeroUntil[SHIELD]( Int.MaxValue );

  abstract class TwosComplementWithBitLength[SHIELD <: Shield]( bitLength : Int ) extends RestrictedInt.MinUntil[SHIELD]( -UnsignedInt(ONE << (bitLength/2)).widen, UnsignedInt(ONE << (bitLength/2)).widen ) {
    require( bitLength % 2 == 0, s"RestrictedInt.TwosComplementWithBitLength requires an even bit length, found ${bitLength}." )
  }
  abstract class TwosComplementWithByteLength[SHIELD <: Shield]( byteLength : Int ) extends RestrictedInt.TwosComplementWithBitLength[SHIELD]( byteLength * 8 )
}
trait RestrictedInt[SHIELD <: RestrictedInt.Shield] extends RestrictedType[CommonConversions.IntegralToInt.type,Int,SHIELD];
