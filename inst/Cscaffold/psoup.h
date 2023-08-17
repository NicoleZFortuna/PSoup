#ifndef PSOUP_H
#define PSOUP_H

/* Declare structures that will be used to hold node and modifier (genetic)
information. Includes the names that will be used to communicate with an
L-System. */
struct NodeVals {
  insertSTRUCTNODENAMES
};

struct GeneVals {
  insertSTRUCTGENENAMES
};

/* Declaring support functions. */
void stepNext(struct GeneVals gen, struct NodeVals* pDat);
int checkSame(struct NodeVals first, struct NodeVals second);
void calculateVals (struct GeneVals gen, struct NodeVals* pDat, int* pTime);

#endif
