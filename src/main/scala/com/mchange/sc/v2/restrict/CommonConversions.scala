package com.mchange.sc.v2.restrict;

object CommonConversions {
  import RestrictedType.Converter;

  // macros would help here.
  // any other means of abstracting over these converters i can think of would box.
  // however, at least we only need Int, Long, and BigInt (as other integrals promote anyway)
  object IntegralToByte {
    implicit object IntByteConverter extends Converter[Nothing,Int,Byte] {
      def convert( i : Int ) : Byte = if (i >= Byte.MinValue && i <= Byte.MaxValue) i.toByte else cannotConvert( i, "Byte" );
    }
    implicit object LongByteConverter extends Converter[Nothing,Long,Byte] {
      def convert( i : Long ) : Byte = if (i >= Byte.MinValue && i <= Byte.MaxValue) i.toByte else cannotConvert( i, "Byte" );
    }
    implicit object BigIntByteConverter extends Converter[Nothing,BigInt,Byte] {
      def convert( i : BigInt ) : Byte = if (i >= Byte.MinValue && i <= Byte.MaxValue) i.toByte else cannotConvert( i, "Byte" );
    }
  }
  object IntegralToShort {
    implicit object IntShortConverter extends Converter[Nothing,Int,Short] {
      def convert( i : Int ) : Short = if (i >= Short.MinValue && i <= Short.MaxValue) i.toShort else cannotConvert( i, "Short" );
    }
    implicit object LongShortConverter extends Converter[Nothing,Long,Short] {
      def convert( i : Long ) : Short = if (i >= Short.MinValue && i <= Short.MaxValue) i.toShort else cannotConvert( i, "Short" );
    }
    implicit object BigIntShortConverter extends Converter[Nothing,BigInt,Short] {
      def convert( i : BigInt ) : Short = if (i >= Short.MinValue && i <= Short.MaxValue) i.toShort else cannotConvert( i, "Short" );
    }
  }
  object IntegralToInt {
    implicit object LongIntConverter extends Converter[Nothing,Long,Int] {
      def convert( i : Long ) : Int = if (i >= Int.MinValue && i <= Int.MaxValue) i.toInt else cannotConvert( i, "Int" );
    }
    implicit object BigIntIntConverter extends Converter[Nothing,BigInt,Int] {
      def convert( i : BigInt ) : Int = if (i >= Int.MinValue && i <= Int.MaxValue) i.toInt else cannotConvert( i, "Int" );
    }
  }
  object IntegralToLong {
    implicit object IntLongConverter extends Converter[Nothing,Int,Long] {
      def convert( i : Int ) : Long = if (i >= Long.MinValue && i <= Long.MaxValue) i.toLong else cannotConvert( i, "Long" );
    }
    implicit object BigIntLongConverter extends Converter[Nothing,BigInt,Long] {
      def convert( i : BigInt ) : Long = if (i >= Long.MinValue && i <= Long.MaxValue) i.toLong else cannotConvert( i, "Long" );
    }
  }
  object IntegralToBigInt {
    implicit object IntBigIntConverter extends Converter[Nothing,Int,BigInt]   { def convert( i : Int ) : BigInt = BigInt( i ); }
    implicit object LongBigIntConverter extends Converter[Nothing,Long,BigInt] { def convert( i : Long ) : BigInt = BigInt( i ); }
  }

  object ToByteSeq {
    import scala.collection._
    import com.mchange.sc.v2.collection.immutable.ImmutableArraySeq;

    implicit object ByteArrayConverter extends Converter[Nothing,Array[Byte],immutable.Seq[Byte]] {
      def convert( arr : Array[Byte] ) : immutable.Seq[Byte] = ImmutableArraySeq.Byte( arr );
    }
    implicit object MutableSeqConverter extends Converter[Nothing,mutable.Seq[Byte],immutable.Seq[Byte]] {
      def convert( seq : mutable.Seq[Byte] ) : immutable.Seq[Byte] = ImmutableArraySeq.Byte.createNoCopy( seq.toArray );
    }
  }
}
