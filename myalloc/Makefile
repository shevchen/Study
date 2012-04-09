libmyalloc.so: large_bucket.c large_bucket.h map.c map.h myalloc.c myalloc.h small_bucket.c small_bucket.h
	gcc -ggdb -shared myalloc.c map.c small_bucket.c large_bucket.c -o libmyalloc.so
