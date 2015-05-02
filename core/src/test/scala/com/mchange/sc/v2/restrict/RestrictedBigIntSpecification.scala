package com.mchange.sc.v2.restrict;

import org.scalacheck.Prop._;
import org.scalacheck.Properties;

object RestrictedBigIntSpecification extends Properties("RestrictedBigInt") {
  import RestrictedBigInt._;

  property("AnyBigInt round trip") = forAll( (bi : BigInt) => bi == AnyBigInt( bi ).widen )

  property("UnsignedBigInt round trip") = forAll{ (bi : BigInt) => 
    bi >= 0 ==> (bi == UnsignedBigInt( bi ).widen)
  }
  property("UnsignedBigInt rejects negative values") = forAll{ (bi : BigInt) => 
    bi < 0 ==> {
      try {UnsignedBigInt( bi ); false} catch { case e : IllegalArgumentException => true }
    }
  }

}
