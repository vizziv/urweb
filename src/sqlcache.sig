signature SQLCACHE = sig

val setCache : Cache.cache -> unit
val getCache : unit -> Cache.cache

val setHeuristic : string -> unit

val getFfiInfo : unit -> Cache.info list
val go : Mono.file -> Mono.file

end
