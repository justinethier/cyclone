/* Makes safe primes of a 2k nature */
#include <tommath.h>
#include <time.h>

static int sizes[] = {256, 512, 768, 1024, 1536, 2048, 3072, 4096};

int main(void)
{
   char buf[2000];
   size_t x;
   int y;
   mp_int q, p;
   FILE *out;
   clock_t t1;
   mp_digit z;

   mp_init_multi(&q, &p, NULL);

   out = fopen("2kprime.1", "w");
   if (out != NULL) {
      for (x = 0; x < (sizeof(sizes) / sizeof(sizes[0])); x++) {
top:
         mp_2expt(&q, sizes[x]);
         mp_add_d(&q, 3uL, &q);
         z = -3;

         t1 = clock();
         for (;;) {
            mp_sub_d(&q, 4uL, &q);
            z += 4uL;

            if (z > MP_MASK) {
               printf("No primes of size %d found\n", sizes[x]);
               break;
            }

            if ((clock() - t1) > CLOCKS_PER_SEC) {
               printf(".");
               fflush(stdout);
               /*            sleep((clock() - t1 + CLOCKS_PER_SEC/2)/CLOCKS_PER_SEC); */
               t1 = clock();
            }

            /* quick test on q */
            mp_prime_is_prime(&q, 1, &y);
            if (y == 0) {
               continue;
            }

            /* find (q-1)/2 */
            mp_sub_d(&q, 1uL, &p);
            mp_div_2(&p, &p);
            mp_prime_is_prime(&p, 3, &y);
            if (y == 0) {
               continue;
            }

            /* test on q */
            mp_prime_is_prime(&q, 3, &y);
            if (y == 0) {
               continue;
            }

            break;
         }

         if (y == 0) {
            ++sizes[x];
            goto top;
         }

         mp_toradix(&q, buf, 10);
         printf("\n\n%d-bits (k = %lu) = %s\n", sizes[x], z, buf);
         fprintf(out, "%d-bits (k = %lu) = %s\n", sizes[x], z, buf);
         fflush(out);
      }
      fclose(out);
   }

   return 0;
}

/* ref:         tag: v1.1.0, master */
/* git commit:  08549ad6bc8b0cede0b357a9c341c5c6473a9c55 */
/* commit time: 2019-01-28 20:32:32 +0100 */
