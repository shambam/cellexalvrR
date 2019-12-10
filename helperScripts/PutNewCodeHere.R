
entrop = function(d) {
  a = table(d)
  ret = 0
   if ( length(a) > 1 ) {
      p = a/sum(a)
      ret = -sum(p*log(p))
   }
 ret
}