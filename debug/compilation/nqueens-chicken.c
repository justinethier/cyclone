/* Generated from nqueens.scm by the CHICKEN compiler
   http://www.call-with-current-continuation.org
   2016-03-04 20:30
   Version 4.7.0 
   linux-unix-gnu-x86 [ manyargs dload ptables ]
   compiled 2011-10-17 on roseapple (Linux)
   command line: nqueens.scm -output-file nqueens.c
   used units: library eval
*/

#include "chicken.h"

static C_PTABLE_ENTRY *create_ptable(void);
C_noret_decl(C_library_toplevel)
C_externimport void C_ccall C_library_toplevel(C_word c,C_word d,C_word k) C_noret;
C_noret_decl(C_eval_toplevel)
C_externimport void C_ccall C_eval_toplevel(C_word c,C_word d,C_word k) C_noret;

static C_TLS C_word lf[4];
static double C_possibly_force_alignment;
static C_char C_TLS li0[] C_aligned={C_lihdr(0,0,17),40,116,114,121,32,120,49,52,32,121,49,53,32,122,49,54,41,0,0,0,0,0,0,0};
static C_char C_TLS li1[] C_aligned={C_lihdr(0,0,27),40,111,107,63,32,114,111,119,49,55,32,100,105,115,116,49,56,32,112,108,97,99,101,100,49,57,41,0,0,0,0,0};
static C_char C_TLS li2[] C_aligned={C_lihdr(0,0,14),40,108,111,111,112,32,105,49,49,32,108,49,50,41,0,0};
static C_char C_TLS li3[] C_aligned={C_lihdr(0,0,12),40,110,113,117,101,101,110,115,32,110,53,41,0,0,0,0};
static C_char C_TLS li4[] C_aligned={C_lihdr(0,0,10),40,116,111,112,108,101,118,101,108,41,0,0,0,0,0,0};


C_noret_decl(C_toplevel)
C_externexport void C_ccall C_toplevel(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_35)
static void C_ccall f_35(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_38)
static void C_ccall f_38(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_207)
static void C_ccall f_207(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_197)
static void C_ccall f_197(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_203)
static void C_ccall f_203(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_200)
static void C_ccall f_200(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_40)
static void C_ccall f_40(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_49)
static void C_fcall f_49(C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_194)
static void C_ccall f_194(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_133)
static void C_fcall f_133(C_word t0,C_word t1,C_word t2,C_word t3,C_word t4) C_noret;
C_noret_decl(f_69)
static void C_fcall f_69(C_word t0,C_word t1,C_word t2,C_word t3,C_word t4) C_noret;
C_noret_decl(f_108)
static void C_ccall f_108(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_115)
static void C_ccall f_115(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_89)
static void C_ccall f_89(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_93)
static void C_ccall f_93(C_word c,C_word t0,C_word t1) C_noret;

C_noret_decl(trf_49)
static void C_fcall trf_49(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_49(void *dummy){
C_word t3=C_pick(0);
C_word t2=C_pick(1);
C_word t1=C_pick(2);
C_word t0=C_pick(3);
C_adjust_stack(-4);
f_49(t0,t1,t2,t3);}

C_noret_decl(trf_133)
static void C_fcall trf_133(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_133(void *dummy){
C_word t4=C_pick(0);
C_word t3=C_pick(1);
C_word t2=C_pick(2);
C_word t1=C_pick(3);
C_word t0=C_pick(4);
C_adjust_stack(-5);
f_133(t0,t1,t2,t3,t4);}

C_noret_decl(trf_69)
static void C_fcall trf_69(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_69(void *dummy){
C_word t4=C_pick(0);
C_word t3=C_pick(1);
C_word t2=C_pick(2);
C_word t1=C_pick(3);
C_word t0=C_pick(4);
C_adjust_stack(-5);
f_69(t0,t1,t2,t3,t4);}

C_noret_decl(tr3)
static void C_fcall tr3(C_proc3 k) C_regparm C_noret;
C_regparm static void C_fcall tr3(C_proc3 k){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
(k)(3,t0,t1,t2);}

C_noret_decl(tr2)
static void C_fcall tr2(C_proc2 k) C_regparm C_noret;
C_regparm static void C_fcall tr2(C_proc2 k){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
(k)(2,t0,t1);}

/* toplevel */
static C_TLS int toplevel_initialized=0;
C_main_entry_point
C_noret_decl(toplevel_trampoline)
static void C_fcall toplevel_trampoline(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall toplevel_trampoline(void *dummy){
C_toplevel(2,C_SCHEME_UNDEFINED,C_restore);}

void C_ccall C_toplevel(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word *a;
if(toplevel_initialized) C_kontinue(t1,C_SCHEME_UNDEFINED);
else C_toplevel_entry(C_text("toplevel"));
C_resize_stack(131072);
C_check_nursery_minimum(3);
if(!C_demand(3)){
C_save(t1);
C_reclaim((void*)toplevel_trampoline,NULL);}
toplevel_initialized=1;
if(!C_demand_2(40)){
C_save(t1);
C_rereclaim2(40*sizeof(C_word), 1);
t1=C_restore;}
a=C_alloc(3);
C_initialize_lf(lf,4);
lf[0]=C_h_intern(&lf[0],7,"nqueens");
lf[1]=C_h_intern(&lf[1],6,"append");
lf[2]=C_h_intern(&lf[2],25,"\003sysimplicit-exit-handler");
lf[3]=C_h_intern(&lf[3],5,"write");
C_register_lf2(lf,4,create_ptable());
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_35,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
C_library_toplevel(2,C_SCHEME_UNDEFINED,t2);}

/* k33 */
static void C_ccall f_35(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_35,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_38,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_eval_toplevel(2,C_SCHEME_UNDEFINED,t2);}

/* k36 in k33 */
static void C_ccall f_38(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[9],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_38,2,t0,t1);}
t2=C_mutate((C_word*)lf[0]+1 /* (set! nqueens ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_40,a[2]=((C_word)li3),tmp=(C_word)a,a+=3,tmp));
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_197,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_207,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
C_trace("nqueens.scm:34: nqueens");
((C_proc3)C_fast_retrieve_symbol_proc(lf[0]))(3,*((C_word*)lf[0]+1),t4,C_fix(8));}

/* k205 in k36 in k33 */
static void C_ccall f_207(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("nqueens.scm:33: write");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[3]+1)))(3,*((C_word*)lf[3]+1),((C_word*)t0)[2],t1);}

/* k195 in k36 in k33 */
static void C_ccall f_197(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_197,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_200,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_203,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace("##sys#implicit-exit-handler");
((C_proc2)C_fast_retrieve_symbol_proc(lf[2]))(2,*((C_word*)lf[2]+1),t3);}

/* k201 in k195 in k36 in k33 */
static void C_ccall f_203(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
t2=t1;
((C_proc2)C_fast_retrieve_proc(t2))(2,t2,((C_word*)t0)[2]);}

/* k198 in k195 in k36 in k33 */
static void C_ccall f_200(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_SCHEME_UNDEFINED);}

/* nqueens in k36 in k33 */
static void C_ccall f_40(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word t11;
C_word t12;
C_word t13;
C_word ab[23],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_40,3,t0,t1,t2);}
t3=C_SCHEME_UNDEFINED;
t4=(*a=C_VECTOR_TYPE|1,a[1]=t3,tmp=(C_word)a,a+=2,tmp);
t5=C_SCHEME_UNDEFINED;
t6=(*a=C_VECTOR_TYPE|1,a[1]=t5,tmp=(C_word)a,a+=2,tmp);
t7=C_set_block_item(t4,0,(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_69,a[2]=t6,a[3]=t4,a[4]=((C_word)li0),tmp=(C_word)a,a+=5,tmp));
t8=C_set_block_item(t6,0,(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_133,a[2]=t6,a[3]=((C_word)li1),tmp=(C_word)a,a+=4,tmp));
t9=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_194,a[2]=t1,a[3]=t4,tmp=(C_word)a,a+=4,tmp);
t10=C_SCHEME_UNDEFINED;
t11=(*a=C_VECTOR_TYPE|1,a[1]=t10,tmp=(C_word)a,a+=2,tmp);
t12=C_set_block_item(t11,0,(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_49,a[2]=t11,a[3]=((C_word)li2),tmp=(C_word)a,a+=4,tmp));
t13=((C_word*)t11)[1];
f_49(t13,t9,t2,C_SCHEME_END_OF_LIST);}

/* loop in nqueens in k36 in k33 */
static void C_fcall f_49(C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word *a;
loop:
a=C_alloc(7);
C_check_for_interrupt;
if(!C_stack_probe(a)){
C_save_and_reclaim((void*)trf_49,NULL,4,t0,t1,t2,t3);}
if(C_truep(C_i_nequalp(t2,C_fix(0)))){
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,t3);}
else{
t4=C_a_i_minus(&a,2,t2,C_fix(1));
t5=C_a_i_cons(&a,2,t2,t3);
C_trace("nqueens.scm:12: loop");
t7=t1;
t8=t4;
t9=t5;
t1=t7;
t2=t8;
t3=t9;
goto loop;}}

/* k192 in nqueens in k36 in k33 */
static void C_ccall f_194(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("nqueens.scm:31: try");
t2=((C_word*)((C_word*)t0)[3])[1];
f_69(t2,((C_word*)t0)[2],t1,C_SCHEME_END_OF_LIST,C_SCHEME_END_OF_LIST);}

/* ok? in nqueens in k36 in k33 */
static void C_fcall f_133(C_word t0,C_word t1,C_word t2,C_word t3,C_word t4){
C_word tmp;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word t11;
C_word t12;
C_word t13;
C_word t14;
C_word t15;
C_word *a;
loop:
a=C_alloc(12);
C_check_for_interrupt;
if(!C_stack_probe(a)){
C_save_and_reclaim((void*)trf_133,NULL,5,t0,t1,t2,t3,t4);}
if(C_truep(C_i_nullp(t4))){
t5=t1;
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,C_SCHEME_TRUE);}
else{
t5=C_i_car(t4);
t6=C_a_i_plus(&a,2,t2,t3);
if(C_truep(C_i_nequalp(t5,t6))){
t7=t1;
((C_proc2)(void*)(*((C_word*)t7+1)))(2,t7,C_SCHEME_FALSE);}
else{
t7=C_i_car(t4);
t8=C_a_i_minus(&a,2,t2,t3);
if(C_truep(C_i_nequalp(t7,t8))){
t9=t1;
((C_proc2)(void*)(*((C_word*)t9+1)))(2,t9,C_SCHEME_FALSE);}
else{
t9=C_a_i_plus(&a,2,t3,C_fix(1));
t10=C_i_cdr(t4);
C_trace("nqueens.scm:29: ok?");
t12=t1;
t13=t2;
t14=t9;
t15=t10;
t1=t12;
t2=t13;
t3=t14;
t4=t15;
goto loop;}}}}

/* try in nqueens in k36 in k33 */
static void C_fcall f_69(C_word t0,C_word t1,C_word t2,C_word t3,C_word t4){
C_word tmp;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word ab[14],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_69,NULL,5,t0,t1,t2,t3,t4);}
if(C_truep(C_i_nullp(t2))){
t5=C_i_nullp(t3);
t6=t1;
((C_proc2)(void*)(*((C_word*)t6+1)))(2,t6,(C_truep(t5)?C_fix(1):C_fix(0)));}
else{
t5=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_89,a[2]=t4,a[3]=((C_word*)t0)[3],a[4]=t3,a[5]=t2,a[6]=t1,tmp=(C_word)a,a+=7,tmp);
t6=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_108,a[2]=t3,a[3]=t5,a[4]=((C_word*)t0)[3],a[5]=t4,a[6]=t2,tmp=(C_word)a,a+=7,tmp);
t7=C_i_car(t2);
C_trace("nqueens.scm:19: ok?");
t8=((C_word*)((C_word*)t0)[2])[1];
f_133(t8,t6,t7,C_fix(1),t4);}}

/* k106 in try in nqueens in k36 in k33 */
static void C_ccall f_108(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_108,2,t0,t1);}
if(C_truep(t1)){
t2=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_115,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[4],a[4]=((C_word*)t0)[5],a[5]=((C_word*)t0)[6],tmp=(C_word)a,a+=6,tmp);
t3=C_i_cdr(((C_word*)t0)[6]);
C_trace("nqueens.scm:20: append");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[1]+1)))(4,*((C_word*)lf[1]+1),t2,t3,((C_word*)t0)[2]);}
else{
t2=((C_word*)t0)[3];
f_89(2,t2,C_fix(0));}}

/* k113 in k106 in try in nqueens in k36 in k33 */
static void C_ccall f_115(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_115,2,t0,t1);}
t2=C_i_car(((C_word*)t0)[5]);
t3=C_a_i_cons(&a,2,t2,((C_word*)t0)[4]);
C_trace("nqueens.scm:20: try");
t4=((C_word*)((C_word*)t0)[3])[1];
f_69(t4,((C_word*)t0)[2],t1,C_SCHEME_END_OF_LIST,t3);}

/* k87 in try in nqueens in k36 in k33 */
static void C_ccall f_89(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_89,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_93,a[2]=t1,a[3]=((C_word*)t0)[6],tmp=(C_word)a,a+=4,tmp);
t3=C_i_cdr(((C_word*)t0)[5]);
t4=C_i_car(((C_word*)t0)[5]);
t5=C_a_i_cons(&a,2,t4,((C_word*)t0)[4]);
C_trace("nqueens.scm:22: try");
t6=((C_word*)((C_word*)t0)[3])[1];
f_69(t6,t2,t3,t5,((C_word*)t0)[2]);}

/* k91 in k87 in try in nqueens in k36 in k33 */
static void C_ccall f_93(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_93,2,t0,t1);}
t2=((C_word*)t0)[3];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_a_i_plus(&a,2,((C_word*)t0)[2],t1));}

#ifdef C_ENABLE_PTABLES
static C_PTABLE_ENTRY ptable[17] = {
{"toplevel:nqueens_2escm",(void*)C_toplevel},
{"f_35:nqueens_2escm",(void*)f_35},
{"f_38:nqueens_2escm",(void*)f_38},
{"f_207:nqueens_2escm",(void*)f_207},
{"f_197:nqueens_2escm",(void*)f_197},
{"f_203:nqueens_2escm",(void*)f_203},
{"f_200:nqueens_2escm",(void*)f_200},
{"f_40:nqueens_2escm",(void*)f_40},
{"f_49:nqueens_2escm",(void*)f_49},
{"f_194:nqueens_2escm",(void*)f_194},
{"f_133:nqueens_2escm",(void*)f_133},
{"f_69:nqueens_2escm",(void*)f_69},
{"f_108:nqueens_2escm",(void*)f_108},
{"f_115:nqueens_2escm",(void*)f_115},
{"f_89:nqueens_2escm",(void*)f_89},
{"f_93:nqueens_2escm",(void*)f_93},
{NULL,NULL}};
#endif

static C_PTABLE_ENTRY *create_ptable(void){
#ifdef C_ENABLE_PTABLES
return ptable;
#else
return NULL;
#endif
}
/* end of file */
