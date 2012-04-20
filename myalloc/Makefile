libmyalloc.so: large_bucket map.c myalloc.c small_bucket
	gcc -ggdb -shared myalloc.c map.c small_bucket large_bucket -o libmyalloc.so
large_bucket: large_bucket.c large_func.c
	gcc -ggdb -shared large_bucket.c large_func.c -o large_bucket
small_bucket: small_bucket.c small_func.c
	gcc -ggdb -shared small_bucket.c small_func.c -o small_bucket
