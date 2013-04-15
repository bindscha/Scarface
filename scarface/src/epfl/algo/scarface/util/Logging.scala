package epfl.algo.scarface.util

import org.slf4j.LoggerFactory

trait Logging {

  val log = LoggerFactory.getLogger(getClass)

  def debug(msg: => String) = if (log.isDebugEnabled) log.debug(msg)

  def info(msg: => String) = if (log.isInfoEnabled) log.info(msg)
  
  def warn(msg: => String) = if (log.isWarnEnabled) log.warn(msg)
  
  def error(msg: => String) = if (log.isErrorEnabled) log.error(msg)
  
  def error(msg: => String, e:Throwable) = if (log.isErrorEnabled) log.error(msg, e)
  
}