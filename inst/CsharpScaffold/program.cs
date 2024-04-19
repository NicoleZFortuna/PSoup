using System;
using System.Diagnostics;

/* Declare structures that will be used to hold node and modifier (genetic)
information. Includes the names that will be used to communicate with an
L-System. */
public struct NodeVals
{
    public float MI;
    public float MBO;
    public float MAux;
    public float MSuc;
    public float MSL;
    public float MCK;

    public NodeVals(float mI, float mBO, float mAux, float mSuc, float mSL, float mCK)
    {
        MI = mI;
        MBO = mBO;
        MAux = mAux;
        MSuc = mSuc;
        MSL = mSL;
        MCK = mCK;
    }
}

public struct GeneVals
{
    public float MAux_M;
    public float MSuc_M;
    public float MSL_M;
    public float MCK_M;

    public GeneVals(float mAux_M, float mSuc_M, float mSL_M, float mCK_M)
    {
        MAux_M = mAux_M;
        MSuc_M = mSuc_M;
        MSL_M = mSL_M;
        MCK_M = mCK_M;
    }
}

class Program
{
    const int TMAX = 100;
    const float THRESHOLD = 0.0004f;

    static NodeVals StepNext(GeneVals gen, NodeVals dat)
    {
        NodeVals oldDat = dat; /* NodeVals object to store the previous timestep. */

        dat.MI = 2 * (oldDat.MSL) / (1 + oldDat.MSuc);
        dat.MBO = 2 * ((oldDat.MCK + oldDat.MSuc) / 2) / (1 + oldDat.MI);
        dat.MAux = (1) * (gen.MAux_M);
        dat.MSuc = (1) * (gen.MSuc_M);
        dat.MSL = (oldDat.MAux) * (gen.MSL_M);
        dat.MCK = (2 * (oldDat.MSuc) / (1 + oldDat.MAux)) * (gen.MCK_M);

        return dat;
    }

    static bool CheckSame(NodeVals oldDat, NodeVals newDat)
    {
        /* If any of the elements contain a significant difference, return false. */
            if (Math.Abs(oldDat.MI - newDat.MI) > THRESHOLD
                || Math.Abs(oldDat.MBO - newDat.MBO) > THRESHOLD
                || Math.Abs(oldDat.MAux - newDat.MAux) > THRESHOLD
                || Math.Abs(oldDat.MSuc - newDat.MSuc) > THRESHOLD
                || Math.Abs(oldDat.MSL - newDat.MSL) > THRESHOLD
                || Math.Abs(oldDat.MCK - newDat.MCK) > THRESHOLD)
                return false;
            else
                /* Return true if no difference was found between node values at */
            return true;
    }

    static NodeVals CalculateVals(GeneVals gen, NodeVals dat)
    {
        NodeVals oldDat = dat; /* NodeVals object to store the previous timestep. */
        NodeVals newDat = dat;
        int check = TMAX / 2; /* A value to communicate to the user that the calculation
        is taking a long time. */

        int pTime = 0;
        while (pTime < TMAX)
        {
            oldDat = newDat;
            /* Calculating out values from old values */
            newDat = StepNext(gen, oldDat);

            /* If stability has been reached, break new of while loop */
            if (pTime > 0 && CheckSame(oldDat, newDat))
            {
                Console.WriteLine("Simulation has reached stability.");
                break;
            }

            /* Throw a warning to the user if half of the available time has already
            been used. */
            if (check == pTime)
            {
                Console.WriteLine($"Warning: half the available time has been used without reaching stability. {pTime}");
            }

            ++pTime;
        }

        Console.WriteLine($"The final node values at time {pTime} are - ");

        return newDat;
    }


    static void Main()
    {
        NodeVals nodeVals = new NodeVals(1,1,1,2,1,1);
        NodeVals finalVals = nodeVals;
        Console.WriteLine("mI is {0}", nodeVals.MI);

        GeneVals geneVals = new GeneVals(1,2,1,1);
        Console.WriteLine("mAux_M is {0}", geneVals.MAux_M);

        finalVals = CalculateVals(geneVals, nodeVals);

        Console.WriteLine($"\tI: {finalVals.MI:F6}, ");
        Console.WriteLine($"\tBO: {finalVals.MBO:F6}, ");
        Console.WriteLine($"\tAux: {finalVals.MAux:F6}, ");
        Console.WriteLine($"\tSuc: {finalVals.MSuc:F6}, ");
        Console.WriteLine($"\tSL: {finalVals.MSL:F6}, ");
        Console.WriteLine($"\tCK: {finalVals.MCK:F6}, ");
    }
}
