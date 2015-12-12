/*
compile with ck in above directory, and using:
gcc test.c -g -lck && ./a.out
*/
#include <ck_hs.h>
#include "../ck/src/ck_ht_hash.h"
#include "include/cyclone/types.h"

static ck_hs_t hs_symbol_table;

static void *hs_malloc(size_t r)
{
    return malloc(r);
}

static void
hs_free(void *p, size_t b, bool r)
{
    (void)b;
    (void)r;
    free(p);
    return;
}

static struct ck_malloc my_allocator = {
    .malloc = hs_malloc,
    .free = hs_free
};

static unsigned long
hs_hash(const void *object, unsigned long seed)
{
  const char *c = object;
  unsigned long h;

  h = (unsigned long)MurmurHash64A(c, strlen(c), seed);
  return h;
}

static bool
hs_compare(const void *previous, const void *compare)
{

  return strcmp(previous, compare) == 0;
}
static void *
set_get(ck_hs_t *hs, const char *value)
{
  unsigned long h;
  void *v;

  h = CK_HS_HASH(hs, hs_hash, value);
  v = ck_hs_get(hs, h, value);
  return v;
}

static bool
set_insert(ck_hs_t *hs, const char *value)
{
  unsigned long h;

  h = CK_HS_HASH(hs, hs_hash, value);
  return ck_hs_put(hs, h, value);
}
//static void *table_get(ck_ht_t *ht, const char *value)
//{
//  ck_ht_entry_t entry;
//  ck_ht_hash_t h;
//  size_t l = strlen(value);
//  void *v = NULL;
//
//  ck_ht_hash(&h, ht, value, l);
//  ck_ht_entry_key_set(&entry, value, l);
//
//  if (ck_ht_get_spmc(ht, h, &entry) == true) {
//    v = ck_ht_entry_value(&entry);
//  }
//  return v;
//}
//
//static int table_insert(ck_ht_t *ht, const char *key, const void *value)
//{
//  ck_ht_entry_t entry;
//  ck_ht_hash_t h;
//  size_t l = strlen(key);
//
//  ck_ht_hash(&h, ht, key, l);
//  ck_ht_entry_set(&entry, h, key, l, "VALUE"); //value);
//  return ck_ht_put_spmc(ht, h, &entry);
//}
//// END CK section
//

char *_strdup (const char *s) {
    char *d = malloc (strlen (s) + 1);
    if (d) { strcpy (d,s); }
    return d;
}

//object find_symbol_by_name(const char *name) {
//  //list l = symbol_table;
//  //for (; !nullp(l); l = cdr(l)) {
//  //  const char *str = symbol_pname(car(l));
//  //  if (strcmp(str, name) == 0) return car(l);
//  //}
//  //return nil;
//  object result = set_get(&hs_symbol_table, name);
//  if (result) {
//    printf("found symbol %s\n", symbol_pname(result));
//  }
//  return result;
//}
//
//object add_symbol(symbol_type *psym) {
//  //symbol_table = mcons(psym, symbol_table);
//  printf("Adding symbol %s\n", symbol_pname(psym));
//  set_insert(&hs_symbol_table, symbol_pname(psym), psym);
//  return psym;
//}
//
//object add_symbol_by_name(const char *name) {
//  symbol_type sym = {{0}, symbol_tag, _strdup(name), nil};
//  symbol_type *psym = malloc(sizeof(symbol_type));
//  memcpy(psym, &sym, sizeof(symbol_type));
//  return add_symbol(psym);
//}
//
//object find_or_add_symbol(const char *name){
//  object sym = find_symbol_by_name(name);
//  if (sym){
//    return sym;
//  } else {
//    return add_symbol_by_name(name);
//  }
//}
//
void main()
{
  char a[] = "a";
  char b[] = "b";
  char c[] = "c";

  if (!ck_hs_init(&hs_symbol_table, 
                  CK_HS_MODE_OBJECT | CK_HS_MODE_SPMC,
                  hs_hash, hs_compare,
                  &my_allocator,
                  2, 43423)){
    fprintf(stderr, "Unable to initialize symbol table\n");
    exit(1);
  }

  
  set_insert(&hs_symbol_table, a);
  printf("hs length = %ld\n", ck_hs_count(&hs_symbol_table));
  printf("has \"a\" = %p\n", set_get(&hs_symbol_table, "a"));
  printf("has \"b\" = %p\n", set_get(&hs_symbol_table, "b"));

  set_insert(&hs_symbol_table, b);
  printf("hs length = %ld\n", ck_hs_count(&hs_symbol_table));
  printf("has \"a\" = %p\n", set_get(&hs_symbol_table, "a"));
  printf("has \"b\" = %p\n", set_get(&hs_symbol_table, "b"));
//  object a = find_or_add_symbol("a");
//  printf("%p\n", a);
//
//  object b = find_or_add_symbol("b");
//  printf("hs length = %ld\n", ck_hs_count(&hs_symbol_table));
//
//  object c = find_or_add_symbol("c");
//  printf("hs length = %ld\n", ck_hs_count(&hs_symbol_table));
//
//  object d = find_or_add_symbol("d");
//  printf("hs length = %ld\n", ck_hs_count(&hs_symbol_table));
//
//  object aa = find_or_add_symbol("a");
//  printf("%p\n", aa);
//  printf("hs length = %ld\n", ck_hs_count(&hs_symbol_table));
  return;
}
