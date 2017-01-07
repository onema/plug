package plug

object CookieTree {
  val empty = CookieTree(Nil,Map.empty)
}

case class CookieTree(cookies: List[Cookie], subTrees: Map[String, CookieTree]) {
  lazy val count: Int = cookies.length + subTrees.map(_._2.count).sum

  lazy val empty: Boolean = count == 0

  def add(cookie: Cookie, segments: List[String]): CookieTree = ???

  //  private void Insert(DreamCookie updatedCookie, string[] segments, int depth) {
  //
  //    // find leaf node
  //    if(depth < segments.Length) {
  //      if(_jars == null) {
  //        _jars = new Dictionary<string, DreamCookieJar>(StringComparer.OrdinalIgnoreCase);
  //      }
  //      DreamCookieJar subjar;
  //      if(!_jars.TryGetValue(segments[depth], out subjar)) {
  //        subjar = new DreamCookieJar();
  //        _jars.Add(segments[depth], subjar);
  //      }
  //      subjar.Insert(updatedCookie, segments, depth + 1);
  //    } else {
  //      if(_cookies == null) {
  //        _cookies = new List<DreamCookie>();
  //      }
  //      List<DreamCookie> expired = new List<DreamCookie>();
  //      for(int i = 0; i < _cookies.Count; ++i) {
  //        DreamCookie cookie = _cookies[i];
  //
  //        // check if cookie is expired; if so, remove it
  //        if(cookie.Expired) {
  //          expired.Add(cookie);
  //          continue;
  //        }
  //
  //        // TODO (steveb): we need to add support for '.' prefixes on the domain name
  //
  //        // check if cookie matches the expired cookie
  //        if(StringUtil.EqualsInvariantIgnoreCase(cookie.Domain, updatedCookie.Domain) && StringUtil.EqualsInvariantIgnoreCase(cookie.Name, updatedCookie.Name) && (cookie.Secure == updatedCookie.Secure)) {
  //          _cookies[i] = updatedCookie;
  //          return;
  //        }
  //      }
  //      foreach(DreamCookie cookie in expired) {
  //        _cookies.Remove(cookie);
  //      }
  //      _cookies.Add(updatedCookie);
  //    }
  //  }
  //
  //  private void Delete(DreamCookie expiredCookie, string[] segments, int depth) {
  //
  //    // find leaf node
  //    if(depth < segments.Length) {
  //      if(_jars != null) {
  //        DreamCookieJar subjar;
  //        if(_jars.TryGetValue(segments[depth], out subjar)) {
  //          subjar.Delete(expiredCookie, segments, depth + 1);
  //          if(subjar.IsEmpty) {
  //            _jars.Remove(segments[depth]);
  //          }
  //        }
  //      }
  //    } else if(_cookies != null) {
  //      List<DreamCookie> expired = new List<DreamCookie>();
  //      foreach(DreamCookie cookie in _cookies) {
  //
  //        // check if cookie is expired; if so, remove it
  //        if(cookie.Expired) {
  //          expired.Add(cookie);
  //          continue;
  //        }
  //
  //        // TODO (steveb): we need to add support for '.' prefixes on the domain name
  //
  //        // check if cookie matches the expired cookie
  //        if(StringUtil.EqualsInvariantIgnoreCase(cookie.Domain, expiredCookie.Domain) && StringUtil.EqualsInvariantIgnoreCase(cookie.Name, expiredCookie.Name) && (cookie.Secure == expiredCookie.Secure)) {
  //          expired.Add(cookie);
  //          continue;
  //        }
  //      }
  //      foreach(DreamCookie cookie in expired) {
  //        _cookies.Remove(cookie);
  //      }
  //    }
  //  }

  def get(segments: List[String]): List[Cookie] = {
    def get(tree: CookieTree, query: List[String], acc: List[Cookie]): List[Cookie] = query match {
      case Nil =>
        // this is the most precise match we could get
        tree.cookies ::: acc
      case head :: tail => tree.subTrees.get(head) match {
        case None =>
          // there are no cookies for a more precise match
          tree.cookies ::: acc
        case Some(subtree) =>
          // we can go deeper
          get(subtree, tail, tree.cookies ::: acc)
      }
    }
    get(this, segments, Nil)
  }
}
