/* Generated by Purgatory 2014-2016 */

package relation.deep

import ch.epfl.data.sc.pardis
import pardis.ir._
import pardis.types.PardisTypeImplicits._
import pardis.effects._
import pardis.deep._
import pardis.deep.scalalib._
import pardis.deep.scalalib.collection._
import pardis.deep.scalalib.io._

import ch.epfl.data.sc.pardis.quasi.anf.{ BaseExt, BaseExtIR }
import ch.epfl.data.sc.pardis.quasi.TypeParameters.MaybeParamTag

trait RelationDSLOps extends Base with RelationOps with RelationScannerOps with ArrayOps with ScalaCoreOps with RelationDSLExtraOps with ch.epfl.data.sc.pardis.quasi.anf.BaseQuasiExp {  
  // Type representation
  val RelationDSLType = RelationDSLIRs.RelationDSLType
  implicit val typeRelationDSL: TypeRep[RelationDSL] = RelationDSLType
  implicit class RelationDSLRep(self : Rep[RelationDSL]) {

  }
  object RelationDSL {

  }
  // constructors

  // IR defs
  
  // method definitions
  
  type RelationDSL = relation.shallow.RelationDSL
}
object RelationDSLIRs extends Base {
  import RelationIRs._
  import RelationScannerIRs._
  import ArrayIRs._
  import ScalaCoreIRs._
  import RelationDSLExtraIRs._
  // Type representation
  case object RelationDSLType extends TypeRep[RelationDSL] {
    def rebuild(newArguments: TypeRep[_]*): TypeRep[_] = RelationDSLType
    val name = "RelationDSL"
    val typeArguments = Nil
  }
      implicit val typeRelationDSL: TypeRep[RelationDSL] = RelationDSLType
  // case classes
  
  type RelationDSL = relation.shallow.RelationDSL
}
trait RelationDSLImplicits extends RelationDSLOps { 
  // Add implicit conversions here!
}
trait RelationDSLComponent extends RelationDSLOps with RelationDSLImplicits {  }

trait RelationDSLPartialEvaluation extends RelationDSLComponent with BasePartialEvaluation {  
  // Immutable field inlining 

  // Mutable field inlining 
  // Pure function partial evaluation
}


// QUASI GENERATED CODE:

object RelationDSLQuasiNodes extends BaseExtIR {
  import RelationDSLIRs._
  import RelationQuasiNodes._
  import RelationScannerQuasiNodes._
  import ArrayQuasiNodes._
  import ScalaCoreQuasiNodes._
  import RelationDSLExtraQuasiNodes._
  // case classes
  type RelationDSL = relation.shallow.RelationDSL
}

trait RelationDSLExtOps extends BaseExt with RelationExtOps with RelationScannerExtOps with ArrayExtOps with ScalaCoreExtOps with RelationDSLExtraExtOps with ch.epfl.data.sc.pardis.quasi.anf.BaseQuasiExt {
  
  import RelationDSLQuasiNodes._
  import ch.epfl.data.sc.pardis.quasi.OverloadHackObj._
  import RelationQuasiNodes._
  import RelationScannerQuasiNodes._
  import ArrayQuasiNodes._
  import ScalaCoreQuasiNodes._
  import RelationDSLExtraQuasiNodes._
  implicit class RelationDSLRep(self : Rep[RelationDSL]) {
  }
  object RelationDSL {
  }
  // constructors
  
  // method definitions
  type RelationDSL = relation.shallow.RelationDSL
}

abstract class RelationDSLOpsPackaged extends RelationDSLOps
abstract class RelationDSLExtOpsPackaged extends RelationDSLExtOps

