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

static void hs_free(void *p, size_t b, bool r)
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

static unsigned long hs_hash(const void *object, unsigned long seed)
{
//  const char *c = object;
//  unsigned long h;
//
//  h = (unsigned long)MurmurHash64A(c, strlen(c), seed);
//  return h;
  const symbol_type *c = object;
  unsigned long h;

  h = (unsigned long)MurmurHash64A(c->pname, strlen(c->pname), seed);
  return h;
}

static bool
hs_compare(const void *previous, const void *compare)
{

  return strcmp((previous), (compare)) == 0;
  //return strcmp(symbol_pname(previous), symbol_pname(compare)) == 0;
}
static void *
set_get(ck_hs_t *hs, const void *value)
{
  unsigned long h;
  void *v;

  h = CK_HS_HASH(hs, hs_hash, value);
  v = ck_hs_get(hs, h, value);
  return v;
}

static bool
set_insert(ck_hs_t *hs, const void *value)
{
  unsigned long h;

  h = CK_HS_HASH(hs, hs_hash, value);
  return ck_hs_put(hs, h, value);
}

char *_strdup (const char *s) {
    char *d = malloc (strlen (s) + 1);
    if (d) { strcpy (d,s); }
    return d;
}

object find_symbol_by_name(const char *name) {
  symbol_type tmp = {{0}, symbol_tag, name, nil};
  object result = set_get(&hs_symbol_table, &tmp);
  if (result) {
    printf("found symbol %s\n", symbol_pname(result));
  }
  return result;
}

object add_symbol(symbol_type *psym) {
  printf("Adding symbol %s\n", symbol_pname(psym));
  set_insert(&hs_symbol_table, psym);
  return psym;
}

object add_symbol_by_name(const char *name) {
  symbol_type sym = {{0}, symbol_tag, _strdup(name), nil};
  symbol_type *psym = malloc(sizeof(symbol_type));
  memcpy(psym, &sym, sizeof(symbol_type));
  return add_symbol(psym);
}

object find_or_add_symbol(const char *name){
  object sym = find_symbol_by_name(name);
  if (sym){
    return sym;
  } else {
    return add_symbol_by_name(name);
  }
}

void main()
{
  char astr[] = "a";
  char bstr[] = "b";
  char cstr[] = "c";
  symbol_type a = {{0}, symbol_tag, astr, nil};
  symbol_type aa = {{0}, symbol_tag, astr, nil};
  symbol_type b = {{0}, symbol_tag, bstr, nil};
  symbol_type c = {{0}, symbol_tag, cstr, nil};

  if (!ck_hs_init(&hs_symbol_table, 
                  CK_HS_MODE_OBJECT | CK_HS_MODE_SPMC,
                  hs_hash, hs_compare,
                  &my_allocator,
                  1024, 43423)){
    fprintf(stderr, "Unable to initialize symbol table\n");
    exit(1);
  }

  
//  set_insert(&hs_symbol_table, &a);
//  printf("hs length = %ld\n", ck_hs_count(&hs_symbol_table));
//  printf("has \"a\" = %p\n", set_get(&hs_symbol_table, &aa));
//  printf("has \"b\" = %p\n", set_get(&hs_symbol_table, &b));
//  printf("has \"c\" = %p\n", set_get(&hs_symbol_table, &c));
//
//  set_insert(&hs_symbol_table, &b);
//  printf("hs length = %ld\n", ck_hs_count(&hs_symbol_table));
//  printf("has \"a\" = %p\n", set_get(&hs_symbol_table, &aa));
//  printf("has \"b\" = %p\n", set_get(&hs_symbol_table, &b));
//  printf("has \"c\" = %p\n", set_get(&hs_symbol_table, &c));

  object asym = find_or_add_symbol("producer");
  printf("%p\n", asym);

  object bsym = find_or_add_symbol("b");
  printf("hs length = %ld\n", ck_hs_count(&hs_symbol_table));

  object csym = find_or_add_symbol("lambda");
  printf("hs length = %ld\n", ck_hs_count(&hs_symbol_table));

  object dsym = find_or_add_symbol("d");
  printf("hs length = %ld\n", ck_hs_count(&hs_symbol_table));

  object aasym = find_or_add_symbol("producer");
  printf("%p\n", aasym);
  printf("hs length = %ld\n", ck_hs_count(&hs_symbol_table));
  return;
}
