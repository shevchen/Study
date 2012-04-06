libmyalloc.so: large_bucket.c large_bucket.h main.c map.c map.h small_bucket.c small_bucket.h
	gcc -ggdb -shared main.c -nostdlib -fno-builtin-free -o libmyalloc.so
