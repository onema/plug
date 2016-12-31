package plug

case class Iff[A](a: A) {
  def orElse(cond: => Boolean)(f: A => A): Iff[A] = if(cond) Iff(f(a)) else this
  def get: A = a
}
