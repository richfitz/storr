## The more complicated case; a list cache.
##
## This is built on top of the object cache but we might need some
## additional support.
##
## The *elements* of a list will get put into the object store as
## before; that's cool.  The extra thing is:
##
## - access the whole list as an R object
## - read attributes of the list
## - access the list by element

## In Redis we can save the list mapping as a redis list:
##
##   RPUSH listname el1 el2 ... eln
##
## where the things that are pushed are the *hashes* of the object,
## stored same as before.  We can treat names specially by storing
## them in a hash perhaps so we can look up the index quickly.
##
## How this interacts with the objects is not actually clear to me.
## The objects are currently stored in one great big hash.  They could
## just as easily be stored in a set of keys though.
##
## If we stored them as keys, then it's simple enough to use TYPE to
## get the underlying type.  That's nice because then the whole thing
## becomes a bit simpler.

## By analogy, in the case of a rds/filesystem, we'd store things in
## directories...

## With the drivers, we can require this as an "extra"; in the case
## where no support is present then fall back on read/save of the
## entire object at once doing the indexing on the R side.  Not
## wonderful.

## The metadata that needs storing is:
##   * names vector along the list
##   * function that can put the whole thing together

## The reason for the latter is because we want to be able to store
## data.frames split into rows or columns and that's going to be the
## same "type" but indexed differently.

## It seems that dumping the *whole* object out and the invalidating
## that on element write is the way to go here, I think.  So we're
## looking at a different namespace entirely.  Getting somewhere!
## This also means that a list with one element has some nice
## properties.

## Then, the next question is *what does the UI look like*?

##   is_list
##   length_list
##
##   set_list
##   get_list
##   del_list
##
##   set_list_info
##   get_list_info
##   del_list_info
