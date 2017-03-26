package xtz.tquant.stra.stralet.impl

import xtz.tquant.stra.stralet.{StraletContext, _}
import java.time.LocalDate


class StraletConfigImpl extends StraletConfig {

    var _context        : StraletContext    = _
    var _parameters     : Map[String, Any]  =  _
    var _universe       : Seq[String]       = _
    var _cycle_interval : Int               = 0

    override def context : StraletContext = _context

    override def parameters : Map[String, Any] = _parameters

    override def universe : Seq[String] = _universe

    override def cycleInterval = _cycle_interval
}



