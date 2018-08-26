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

object CommonConversions {
  import RestrictedType.Converter;

  // macros would help here.
  // any other means of abstracting over these converters i can think of would box.
  object IntegralToByte {
    implicit object ShortByteConverter extends Converter[Nothing,Short,Byte] {
      def convert( i : Short ) : Byte = if (i >= Byte.MinValue && i <= Byte.MaxValue) i.toByte else cannotConvert( i, "Byte" );
    }
    implicit object IntByteConverter extends Converter[Nothing,Int,Byte] {
      def convert( i : Int ) : Byte = if (i >= Byte.MinValue && i <= Byte.MaxValue) i.toByte else cannotConvert( i, "Byte" );
    }
    implicit object LongByteConverter extends Converter[Nothing,Long,Byte] {
      def convert( i : Long ) : Byte = if (i >= Byte.MinValue && i <= Byte.MaxValue) i.toByte else cannotConvert( i, "Byte" );
    }
    implicit object BigIntByteConverter extends Converter[Nothing,BigInt,Byte] {
      def convert( i : BigInt ) : Byte = if (i >= Byte.MinValue && i <= Byte.MaxValue) i.toByte else cannotConvert( i, "Byte" );
    }
    implicit object BigIntegerByteConverter extends Converter[Nothing,java.math.BigInteger,Byte] {
      def convert( i : java.math.BigInteger ) : Byte = BigIntByteConverter.convert( BigInt(i) );
    }
  }
  object IntegralToShort {
    implicit object ByteShortConverter extends Converter[Nothing,Byte,Short] {
      def convert( i : Byte ) : Short = if (i >= Short.MinValue && i <= Short.MaxValue) i.toShort else cannotConvert( i, "Short" );
    }
    implicit object IntShortConverter extends Converter[Nothing,Int,Short] {
      def convert( i : Int ) : Short = if (i >= Short.MinValue && i <= Short.MaxValue) i.toShort else cannotConvert( i, "Short" );
    }
    implicit object LongShortConverter extends Converter[Nothing,Long,Short] {
      def convert( i : Long ) : Short = if (i >= Short.MinValue && i <= Short.MaxValue) i.toShort else cannotConvert( i, "Short" );
    }
    implicit object BigIntShortConverter extends Converter[Nothing,BigInt,Short] {
      def convert( i : BigInt ) : Short = if (i >= Short.MinValue && i <= Short.MaxValue) i.toShort else cannotConvert( i, "Short" );
    }
    implicit object BigIntegerShortConverter extends Converter[Nothing,java.math.BigInteger,Short] {
      def convert( i : java.math.BigInteger ) : Short = BigIntShortConverter.convert( BigInt(i) );
    }
  }
  object IntegralToInt {
    implicit object ByteIntConverter extends Converter[Nothing,Byte,Int] {
      def convert( i : Byte ) : Int = if (i >= Int.MinValue && i <= Int.MaxValue) i.toInt else cannotConvert( i, "Int" );
    }
    implicit object ShortIntConverter extends Converter[Nothing,Short,Int] {
      def convert( i : Short ) : Int = if (i >= Int.MinValue && i <= Int.MaxValue) i.toInt else cannotConvert( i, "Int" );
    }
    implicit object LongIntConverter extends Converter[Nothing,Long,Int] {
      def convert( i : Long ) : Int = if (i >= Int.MinValue && i <= Int.MaxValue) i.toInt else cannotConvert( i, "Int" );
    }
    implicit object BigIntIntConverter extends Converter[Nothing,BigInt,Int] {
      def convert( i : BigInt ) : Int = if (i >= Int.MinValue && i <= Int.MaxValue) i.toInt else cannotConvert( i, "Int" );
    }
    implicit object BigIntegerIntConverter extends Converter[Nothing,java.math.BigInteger,Int] {
      def convert( i : java.math.BigInteger ) : Int = BigIntIntConverter.convert( BigInt(i) );
    }
  }
  object IntegralToLong {
    implicit object ByteLongConverter extends Converter[Nothing,Byte,Long] {
      def convert( i : Byte ) : Long = if (i >= Long.MinValue && i <= Long.MaxValue) i.toLong else cannotConvert( i, "Long" );
    }
    implicit object ShortLongConverter extends Converter[Nothing,Short,Long] {
      def convert( i : Short ) : Long = if (i >= Long.MinValue && i <= Long.MaxValue) i.toLong else cannotConvert( i, "Long" );
    }
    implicit object IntLongConverter extends Converter[Nothing,Int,Long] {
      def convert( i : Int ) : Long = if (i >= Long.MinValue && i <= Long.MaxValue) i.toLong else cannotConvert( i, "Long" );
    }
    implicit object BigIntLongConverter extends Converter[Nothing,BigInt,Long] {
      def convert( i : BigInt ) : Long = if (i >= Long.MinValue && i <= Long.MaxValue) i.toLong else cannotConvert( i, "Long" );
    }
    implicit object BigIntegerLongConverter extends Converter[Nothing,java.math.BigInteger,Long] {
      def convert( i : java.math.BigInteger ) : Long = BigIntLongConverter.convert( BigInt(i) );
    }
  }
  object IntegralToBigInt {
    implicit object ByteBigIntConverter extends Converter[Nothing,Byte,BigInt]   { def convert( i : Byte ) : BigInt = BigInt( i ); }
    implicit object ShortBigIntConverter extends Converter[Nothing,Short,BigInt] { def convert( i : Short ) : BigInt = BigInt( i ); }
    implicit object IntBigIntConverter extends Converter[Nothing,Int,BigInt]     { def convert( i : Int ) : BigInt = BigInt( i ); }
    implicit object LongBigIntConverter extends Converter[Nothing,Long,BigInt]   { def convert( i : Long ) : BigInt = BigInt( i ); }
    implicit object BigIntegerBigIntConverter extends Converter[Nothing,java.math.BigInteger,BigInt] { 
      def convert( i : java.math.BigInteger ) : BigInt = BigInt( i ); 
    }
  }

  object ToByteSeq {
    import scala.collection._
    import com.mchange.sc.v2.collection.immutable.ImmutableArraySeq;

    implicit object ByteArrayConverter extends Converter[Nothing,Array[Byte],immutable.Seq[Byte]] {
      def convert( arr : Array[Byte] ) : immutable.Seq[Byte] = ImmutableArraySeq.Byte( arr ); // we do need a copy here
    }
    implicit object MaybeMutableSeqConverter extends Converter[Nothing,Seq[Byte],immutable.Seq[Byte]] {
      def convert( seq : Seq[Byte] ) : immutable.Seq[Byte] = ImmutableArraySeq.Byte( seq.toArray ); //we do need a copy here too, consider e.g. WrappedArray
    }
  }

  object ToString {
    // no automatic conversions
  }
}
