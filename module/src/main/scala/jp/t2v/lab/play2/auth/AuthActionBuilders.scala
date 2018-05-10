package jp.t2v.lab.play2.auth

import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}
//import scala.concurrent.ExecutionContext.global
trait AuthActionBuilders extends AsyncAuth { self: AuthConfig with BaseControllerHelpers =>

  final case class GenericOptionalAuthRequest[+A, R[+_] <: Request[_]](user: Option[User], underlying: R[A]) extends WrappedRequest[A](underlying.asInstanceOf[Request[A]])
  final case class GenericAuthRequest[+A, R[+_] <: Request[_]](user: User, underlying: R[A]) extends WrappedRequest[A](underlying.asInstanceOf[Request[A]])

  final case class GenericOptionalAuthFunction[R[+_] <: Request[_]](implicit context: ExecutionContext) extends ActionFunction[R, ({type L[+A] = GenericOptionalAuthRequest[A, R]})#L] {
    val executionContext: ExecutionContext = context
    def invokeBlock[A](request: R[A], block: GenericOptionalAuthRequest[A, R] => Future[Result]) = {
  //    implicit val ctx = executionContext
      restoreUser(request.asJava.asScala(), executionContext) recover {
        case _ => None -> identity[Result] _
      } flatMap { case (user, cookieUpdater) =>
        block(GenericOptionalAuthRequest[A, R](user, request)).map(cookieUpdater)
      }
    }
  }

  final case class GenericAuthenticationRefiner[R[+_] <: Request[_]](implicit context: ExecutionContext) extends ActionRefiner[({type L[+A] = GenericOptionalAuthRequest[A, R]})#L, ({type L[+A] = GenericAuthRequest[A, R]})#L] {
    val executionContext: ExecutionContext = context
    override protected def refine[A](request: GenericOptionalAuthRequest[A, R]): Future[Either[Result, GenericAuthRequest[A, R]]] = {
      request.user map { user =>
        Future.successful(Right[Result, GenericAuthRequest[A, R]](GenericAuthRequest[A, R](user, request.underlying)))
      } getOrElse {
//        implicit val ctx = executionContext
        authenticationFailed(request).map(Left.apply[Result, GenericAuthRequest[A, R]])
      }
    }
  }

  final case class GenericAuthorizationFilter[R[+_] <: Request[_]](authority: Authority)(implicit context: ExecutionContext) extends ActionFilter[({type L[+B] = GenericAuthRequest[B, R]})#L] {
    val executionContext: ExecutionContext = context
    override protected def filter[A](request: GenericAuthRequest[A, R]): Future[Option[Result]] = {
      //implicit val ctx = executionContext
      authorize(request.user, authority) collect {
        case true => None
      } recoverWith {
        case _ => authorizationFailed(request, request.user, Some(authority)).map(Some.apply)
      }
    }
  }

  final def composeOptionalAuthAction[R[+_] <: Request[_]](builder: ActionBuilder[R, AnyContent]): ActionBuilder[({type L[+A] = GenericOptionalAuthRequest[A, R]})#L, AnyContent] = {
    builder.andThen[({type L[A] = GenericOptionalAuthRequest[A, R]})#L](GenericOptionalAuthFunction[R]()(controllerComponents.executionContext))
  }

  final def composeAuthenticationAction[R[+_] <: Request[_]](builder: ActionBuilder[R, AnyContent]): ActionBuilder[({type L[+A] = GenericAuthRequest[A, R]})#L, AnyContent] = {
    composeOptionalAuthAction[R](builder).andThen[({type L[+A] = GenericAuthRequest[A, R]})#L](GenericAuthenticationRefiner[R]()(controllerComponents.executionContext))
  }

  final def composeAuthorizationAction[R[+_] <: Request[_]](builder: ActionBuilder[R, AnyContent])(authority: Authority): ActionBuilder[({type L[+A] = GenericAuthRequest[A, R]})#L, AnyContent] = {
    composeAuthenticationAction(builder).andThen[({type L[+A] = GenericAuthRequest[A, R]})#L](GenericAuthorizationFilter[R](authority)(controllerComponents.executionContext))
  }

  final type OptionalAuthRequest[+A] = GenericOptionalAuthRequest[A, Request]
  final type AuthRequest[+A] = GenericAuthRequest[A, Request]
  final val OptionalAuthFunction: ActionFunction[Request, OptionalAuthRequest] = GenericOptionalAuthFunction[Request]()(controllerComponents.executionContext)
  final val AuthenticationRefiner: ActionRefiner[OptionalAuthRequest, AuthRequest] = GenericAuthenticationRefiner[Request]()(controllerComponents.executionContext)
  final def AuthorizationFilter(authority: Authority): ActionFilter[AuthRequest] = GenericAuthorizationFilter[Request](authority)(controllerComponents.executionContext)

  final val OptionalAuthAction: ActionBuilder[OptionalAuthRequest,AnyContent] = composeOptionalAuthAction(Action)
  final val AuthenticationAction: ActionBuilder[AuthRequest, AnyContent] = composeAuthenticationAction(Action)
  final def AuthorizationAction(authority: Authority): ActionBuilder[AuthRequest,AnyContent] = composeAuthorizationAction(Action)(authority)

}
