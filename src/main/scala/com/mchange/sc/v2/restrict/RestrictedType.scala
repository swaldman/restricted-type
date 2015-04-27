package com.mchange.sc.v2.restrict;

object RestrictedType {
  trait MinUntil {
    self : RestrictedType[_,_,_] =>

    val MinValueInclusive : Any;
    val MaxValueExclusive : Any;
    override def mathRep : String = s"[${MinValueInclusive},${MaxValueExclusive})"
  }
  trait Element[BELLY] extends Any {
    def value : BELLY;
    def unwrap : BELLY = value;

    override def toString : String = {
      import javax.xml.bind.DatatypeConverter;

      val valueStr = value match {
        case ba : Array[Byte]                                                 => s"0x${DatatypeConverter.printHexBinary( ba )}";
        case bs : Seq[Byte @unchecked] if (bs.forall( _.isInstanceOf[Byte] )) => s"0x${DatatypeConverter.printHexBinary( bs.toArray )}";
        case other                                                            => String.valueOf( other );
      }
      s"${this.getClass.getSimpleName()}(${valueStr})"
    }
  }
  trait Converter[+SEARCHME,T,BELLY] {
    def convert( t : T ) : BELLY; // may throw a CannotConvertException, implies that t cannot be converted, and so cannot belong to any restriction on BELLY
  }

  private val RecoverFalse : PartialFunction[Throwable,Boolean] = { case e : CannotConvertException => false }
  private val RecoverIAE   : PartialFunction[Throwable,Nothing] = { case e : CannotConvertException => throw new IllegalArgumentException( e.getMessage, e ) }
}
trait RestrictedType[SEARCHME, BELLY, SHIELD <: AnyVal] {
  import RestrictedType.{Converter, RecoverFalse, RecoverIAE};

  protected def create( b : BELLY ) : SHIELD;

  def contains( b : BELLY ) : Boolean;
  def contains[T]( xb : T )( implicit converter : Converter[SEARCHME,T,BELLY] ) : Boolean = try this.contains( converter.convert( xb ) ) catch RecoverFalse;

  final def elem_:  ( b : BELLY ) : Boolean = this.contains( b );
  final def elem_!: ( b : BELLY ) : Boolean = this.contains( b ) || ( throw new IllegalArgumentException( badValueMessage( b ) ) );

  final def elem_:[T]  ( xb : T )( implicit converter : Converter[SEARCHME,T,BELLY] ) : Boolean = try this.elem_:( converter.convert( xb ) )  catch RecoverFalse;
  final def elem_!:[T] ( xb : T )( implicit converter : Converter[SEARCHME,T,BELLY] ) : Boolean = try this.elem_!:( converter.convert( xb ) ) catch RecoverIAE; 

  final def apply[T]( xb : T )( implicit converter : Converter[SEARCHME,T,BELLY] ) : SHIELD = {
    apply( converter.convert( xb ) )
  }

  final def apply( b : BELLY ) : SHIELD = {
    require( b elem_!: this );
    create( b );
  }

  //final def unsafe( b : Belly ) : SHIELD = create(b);

  val simpleName : String = try { 
    this.getClass.getSimpleName.filter( _ != '$' ) 
  } catch { // workaround Scala bug, "java.lang.InternalError: Malformed class name"
    case e : Throwable => {
      val fqcn = this.getClass.getName;
      fqcn.reverse.dropWhile( _ == '$' ).takeWhile( "$.".indexOf(_) < 0 ).reverse
    }
  }
  def mathRep : String = simpleName;
  override def toString : String = {
    val mr = mathRep;
    if ( mr == simpleName ) simpleName else s"${simpleName}: ${mathRep}";
  }
  def notMemberMessage( a : Any ) = s"${a} \u2209 ${mathRep}";
  def badValueMessage( a : Any ) = "Bad value: " + notMemberMessage( a )
}
