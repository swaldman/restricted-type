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

import scala.collection.immutable;

object RestrictedByteSeq {
  type ShieldType = AnyVal with RestrictedType.Element[immutable.Seq[Byte]];

  // 2.11 compiler inadequacy, does not recognize ShieldType as an extendable type, alas.

  object AnyByteSeq extends RestrictedByteSeq[AnyByteSeq] {
    override def contains( value : immutable.Seq[Byte] ) : Boolean = true;
    override protected def create( value : immutable.Seq[Byte] ) = new AnyByteSeq( value ); 
  }
  class AnyByteSeq private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];

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
