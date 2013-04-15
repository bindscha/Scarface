package epfl.algo.scarface.network

import epfl.algo.scarface.util.Logging

/**
 * <code>AbstractRequestHandlerFactory</code> represents a specific request handler factory object for GGP.
 * The GGP specification provides 4 request types: PLAY, START, STOP and KILL which are all created by this class.
 */
class RequestHandlerFactory extends AbstractRequestHandlerFactory {
  
  import java.net.Socket
  
  import epfl.algo.scarface.network.{StartRequestHandler, StopRequestHandler, PlayRequestHandler, KillRequestHandler}
  import epfl.algo.scarface.gdl.Gdl._
  
  // TODO: better design would to pass in parsed/checked/prepared arguments to child...
  override protected def requestHandlerNew(_socket : Socket, _header : HTTPHeader, _list : GdlList) : AbstractRequestHandler = _list match {
	  case GdlList(List(list @ GdlList(GdlAtom(s) :: GdlAtom(Symbol(id)) :: _))) if s == 'start => new StartRequestHandler(_socket, _header, list, id)
      case GdlList(List(list @ GdlList(GdlAtom(s) :: GdlAtom(Symbol(id)) :: _))) if s == 'stop => new StopRequestHandler(_socket, _header, list, id)
      case GdlList(List(list @ GdlList(GdlAtom(s) :: GdlAtom(Symbol(id)) :: _))) if s == 'play => new PlayRequestHandler(_socket, _header, list, id)
      case GdlList(List(list @ GdlList(GdlAtom(s) :: GdlAtom(Symbol(id)) :: _))) if s == 'kill => new KillRequestHandler(_socket, _header, list, id)
      case GdlList(List(list @ GdlList(GdlAtom(s) :: GdlAtom(Symbol(id)) :: _))) => throw new IllegalArgumentException("Game [" + id + "] No handler for request type " + s)
  }
  
}
