libmyalloc.so: large_bucket.c large_bucket.h large_func.c large_func.h map.c map.h myalloc.c myalloc.h small_bucket.c small_bucket.h small_func.c small_func.h
	gcc -ggdb -shared myalloc.c map.c small_bucket.c small_func.c large_bucket.c large_func.c -o libmyalloc.so
