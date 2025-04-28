#include <stdio.h>
#include "psoup.h"

int main () {
    int time;

    /* For collecting starting info. */
    struct NodeVals dat;
    struct GeneVals gen;
    insertEXOmainDef;

  insertDATVALS

  insertGENEVALS
  insertEXOVALS

    /* Calculate values. */
    calculateVals(gen, &datinsertEXOtype, &time);

    fprintf(stderr,"The final node values at time %d are - \n\t", time);
    insertFINALPRINT;

    return 0;
}
