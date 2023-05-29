#include <stdio.h>
#include <math.h>

/* Define the maximum timesteps that you want to run in case of an infinite loop. */
#define TMAX 10

/* Define structures that will be used to hold node and modifier (genetic) 
information. Defines the names that will be used to communicate with an
L-System. */
struct nodeVals {
    float GreenLight;
    float Go;
};

struct geneVals {
    float on;
};

/* Giving the starting conditions for a simulation. */
struct nodeVals dat = {3.1,2.1};
struct geneVals gen = {1};

/* Declaring support functions. */
void nextStep(struct nodeVals *old, struct geneVals *gen, struct nodeVals *new);
int noChange(struct nodeVals *first, struct nodeVals *second);
void transferVals(struct nodeVals *old, struct nodeVals *new);
void printFinal(struct nodeVals *new, int t);

/* This function calculates the new element values based on the element values of 
the previous time step */
void nextStep(struct nodeVals *old, struct geneVals *gen, struct nodeVals *new) {
    new->GreenLight = 1 * gen->on;
    new->Go = old->GreenLight;
}

/* This function checks if the elements contained in the old and new struct objects
contain the same values after incrementing the simulation by one time step. */
int noChange(struct nodeVals *old, struct nodeVals *new) {
    #define THRESHOLD 0.0001 /* Threshold of accuracy for matching values. */

    /* If any of the elements contain a significant difference, return 0. */
    if (fabs(old->Go - new->Go) > THRESHOLD) {
        return 0;
    } else if (fabs(old->GreenLight - new->GreenLight) > THRESHOLD) {
        return 0;
    }

    /* Return 1 if no difference was found between node values at */
    return 1;
}

/* This function is used to print the final node values of subsequent 
timesteps. */
void printFinal(struct nodeVals *new, int t) {
    printf("The final node values at time %d are...\nGreenLight: %.2f\nGo: %.2f\n", t, new->GreenLight, new->Go);
}

/* This function moves values contained in the new struct object 
to the old struct object. Does this by swapping the pointers between 
new and old. This makes sure that the new pointer does not have the 
same value as the old pointer. */
void transferVals(struct nodeVals *old, struct nodeVals *new) {
    struct nodeVals temp = *old;

    *old = *new;
    *new = temp;
}

int main () {
    extern struct nodeVals dat; /* Collecting starting info. */
    extern struct geneVals gen; /* Collecting starting info. */
    struct nodeVals new; /* nodeVals object to collect the next timestep. */
    struct nodeVals old; /* nodeVals object to store the previous timestep. */
    int i = 0;
    int j;
    int check = TMAX/2; /* A value to communictate to the user that the 
    is taking a long time. */
    
    /* Transfering values in dat to old */
    transferVals(&old, &dat);

    while (i < TMAX) {
        /* Calculating out values from old values */
        nextStep(&old, &gen, &new);

        /* If stability has been reached, break new of while loop */
        if (i > 0 && noChange(&old, &new) == 1) {
            printf("Simulation has reached stability.\n");
            break;
        }

        /* Throw a waring to the user if half of the available time has already 
        been used. */
        if (check == i) {
            printf("Warning: half the available time has been used without reaching stability. %d\n", i);
        }
        
        /* Transfering values in new to old */
        transferVals(&old, &new);

        ++i;
    }

    /* Printing final times */
    printFinal(&new, i);

    return 0;
}
