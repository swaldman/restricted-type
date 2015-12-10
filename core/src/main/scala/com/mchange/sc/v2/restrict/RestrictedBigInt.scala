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

object RestrictedBigInt {
  type Shield = AnyVal with RestrictedType.Element[BigInt];

  // 2.11 compiler inadequacy, does not recognize Shield as an extendable type, alas.

  object AnyBigInt extends RestrictedBigInt[AnyBigInt] {
    override def contains( value : BigInt ) : Boolean = true;
    override protected def create( value : BigInt ) = new AnyBigInt( value ); 
  }
  final class AnyBigInt private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];

  object UnsignedBigInt extends Unsigned[UnsignedBigInt] {
    override protected def create( value : BigInt ) = new UnsignedBigInt( value ); 
  }
  final class UnsignedBigInt private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];

  abstract class MinUntil[SHIELD <: Shield]( val MinValueInclusive : BigInt, val MaxValueExclusive : BigInt ) extends RestrictedBigInt[SHIELD] with RestrictedType.MinUntil {
    require( MaxValueExclusive > MinValueInclusive );
    override def contains( value : BigInt ) : Boolean = value >= MinValueInclusive && value < MaxValueExclusive;
  }
  abstract class ZeroUntil[SHIELD <: Shield]( max : BigInt ) extends RestrictedBigInt.MinUntil[SHIELD](0, max );
  abstract class UnsignedWithBitLength[SHIELD <: Shield]( bitLength : Int ) extends RestrictedBigInt.ZeroUntil[SHIELD]( AnyBigInt(ONE << bitLength).widen );
  abstract class UnsignedWithByteLength[SHIELD <: Shield]( byteLength : Int ) extends UnsignedWithBitLength[SHIELD]( byteLength * 8 );
  abstract class Unsigned[SHIELD <: Shield] extends RestrictedBigInt[SHIELD] {
    override def contains( value : BigInt ) : Boolean = value >= 0;
    override def mathRep = "[0,\u221E)";
  }
}
trait RestrictedBigInt[SHIELD <: RestrictedBigInt.Shield] extends RestrictedType[CommonConversions.IntegralToBigInt.type,BigInt,SHIELD];
