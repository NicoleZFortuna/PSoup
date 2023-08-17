#include <stdio.h>
#include <math.h>
#include "psoup.h"
/* Define the maximum timesteps that you want to run in case of an infinite loop. */
#define TMAX insertTMAX;
/* Threshold of accuracy for matching values. */
#define THRESHOLD insertTHRESHOLD;

/* This function calculates the new element values based on the element values of
the previous time step */
void stepNext(struct GeneVals gen, struct NodeVals* pDat) {

    struct NodeVals oldDat = *pDat; /* NodeVals object to store the previous timestep. */

    insertEQUATIONS;
}

/* This function checks if the elements contained in the old and new struct objects
contain the same values after incrementing the simulation by one time step. */
int checkSame(struct NodeVals oldDat, struct NodeVals newDat) {
    /* If any of the elements contain a significant difference, return 0. */
    if (insertCOMPARISONCHAIN)
        return 0;
    else
        /* Return 1 if no difference was found between node values at */
        return 1;
}

void calculateVals (struct GeneVals gen, struct NodeVals* pDat, int* pTime) {

    struct NodeVals oldDat; /* NodeVals object to store the previous timestep. */
    int check = TMAX/2; /* A value to communicate to the user that the calculation
    is taking a long time. */

    *pTime = 0;
    while (*pTime < TMAX) {
        /* Transferring values in new to old */
        oldDat = *pDat;

        /* Calculating out values from old values */
        stepNext(gen, pDat);

        /* If stability has been reached, break new of while loop */
        if (*pTime > 0 && checkSame(oldDat, *pDat)) {
            fprintf(stderr,"Simulation has reached stability.\n");
            break;
        }

        /* Throw a warning to the user if half of the available time has already
        been used. */
        if (check == *pTime) {
            fprintf(stderr,"Warning: half the available time has been used without reaching stability. %d\n", *pTime);
        }

        ++*pTime;
    }
}
