using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.Collections;
using System.Windows.Forms;

namespace WEB30
{
    public class Input
    {
        private string[] inputArray, batchArray;
        private string[][] sortInputs;

        const int nyearsprop = 5;
        const int nacrsyrs = 8;

        public string title, region, batchfile, resultsfile, pricefile, outputFileName;
        public double royalty, leasecost, rental, privdis, socdis, inflation, startprice, trendstart, sigma, realtrend, pulltotrend,
            drift, tangportion, tax, fieldsd, fieldsize, probsucc, declinerate, pdisnom, sdisnom, distime, bk, ck, dk, alpha, beta,
            reffield, ftract, trancost, opcost, explcost, ffac1, ffac2, rsp1, rsp2, hurdleprice;
        public int year, pricegrid, reservegrid, delayyears, delayincr, printsize, batchmode, periods, ntsl80areas, explterm, yrfirstexpl,
            yrtodev, maxyrsdecline, yearstopeak, minatpeak, maxatpeak, besttimetop, pplus1, pplusdelay, produceyears, prodlag;
        private decision explDecision, eDecision, aDecision, wDecision;

        public string currDirectory = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetEntryAssembly().Location);

        public int userNumberLoops = 10, compareYear = 5;
        public double userStartPrice = 20, userIncrement = 1;       // overwritten by webform, hardcoded for testing

        public string currentRunDateTime;
        public int viewer = 0;

        public double[] acrs = new double[nacrsyrs];
        public double[] propdevcost = new double[nyearsprop];
        private double[][] tslArrayFirst, tslArraySecond;
        private string[] tslArrayNames;

        private double[,] delayresults, prices, pricesreal, probexplore, probdevelop, fielddata;
        private double[, ,] priceprob, exprices, expricesreal;
        private double[] errors, trendprice, delayphold;
        private decision[, ,] solution;

        public string[] outputArray53;
        public List<string> resultsViewer53 = new List<string>();

        // 6/22/2017 update OECM Social Cost Subtraction & recalc
        public List<double> OECMSocialMod = new List<double>();
        public List<List<double>> socialCostsIncremented = new List<List<double>>();
        public List<List<double>> socialCostsSubtractOECM = new List<List<double>>();

        public struct decision
        {
            public string action;
            public double privvalue;
            public double socialval;
            
            public decision(string a, double b, double c)
            {
                action = a;
                privvalue = b;
                socialval = c;
            }
            public decision(string a)
            {
                action = a;
                privvalue = 0;
                socialval = 0;
            }
        }
        public void readInputs(string fileName)
        {
            inputArray = System.IO.File.ReadAllLines(fileName);
        }
        private void readBatchInputs(string fileName)
        {
            batchArray = System.IO.File.ReadAllLines(fileName);
        }
        public void writeOutput(string[] stringArray53, string outfile, int loadviewer, int printFinal)
        {
            if (printFinal == 1)
                System.IO.File.WriteAllLines(outfile, stringArray53);
            if (loadviewer == 1 && printFinal == 1)
            {
                string title = "WEB 3.0 Results Viewer";
                ResultsViewer loadForm = new ResultsViewer(stringArray53, title);
                loadForm.Show();
            }
            outputArray53 = stringArray53;
            
        }
        public string[][] convertInputs(char[] delimiterChars, string[] iArray)
        {
            string[][] sortedInputs = new string[iArray.Length][];
            string[] splitString;
            for (int i = 0; i < iArray.Length; i++)
            {
                splitString = iArray[i].Split(delimiterChars, StringSplitOptions.RemoveEmptyEntries);
                for (int j = 0; j < splitString.Length; j++)
                    sortedInputs[i] = splitString;
            }
            return sortedInputs;
        }
        private void assignInputs()
        {
            // this function reads and assigns variables from WEB2 input file. 
            // Indexes are based on input file location.

            title = sortInputs[0][0];
            year = int.Parse(sortInputs[2][0]);
            region = sortInputs[3][0];
            pricegrid = int.Parse(sortInputs[4][0]);
            reservegrid = int.Parse(sortInputs[5][0]);
            delayyears = int.Parse(sortInputs[6][0]);
            delayincr = int.Parse(sortInputs[7][0]);
            printsize = int.Parse(sortInputs[8][0]);
            batchmode = int.Parse(sortInputs[9][0]);
            batchfile = sortInputs[10][0];
            pricefile = sortInputs[12][0];
            resultsfile = sortInputs[13][0];
            periods = int.Parse(sortInputs[15][0]);
            explterm = int.Parse(sortInputs[16][0]);
            royalty = double.Parse(sortInputs[17][0]);
            leasecost = double.Parse(sortInputs[18][0]);
            rental = double.Parse(sortInputs[19][0]);
            yrfirstexpl = int.Parse(sortInputs[20][0]);
            yrtodev = int.Parse(sortInputs[21][0]);
            privdis = double.Parse(sortInputs[23][0]);
            socdis = double.Parse(sortInputs[24][0]);
            distime = int.Parse(sortInputs[25][0]);
            inflation = double.Parse(sortInputs[26][0]);
            startprice = double.Parse(sortInputs[27][0]);
            trendstart = double.Parse(sortInputs[28][0]);
            sigma = double.Parse(sortInputs[29][0]);
            realtrend = double.Parse(sortInputs[30][0]);
            pulltotrend = double.Parse(sortInputs[31][0]);
            drift = double.Parse(sortInputs[32][0]);
            tangportion = double.Parse(sortInputs[34][0]);
            tax = double.Parse(sortInputs[35][0]);
            fieldsd = double.Parse(sortInputs[37][0]);
            fieldsize = double.Parse(sortInputs[38][0]);
            probsucc = double.Parse(sortInputs[39][0]);
            declinerate = double.Parse(sortInputs[40][0]);
            maxyrsdecline = int.Parse(sortInputs[41][0]);
            yearstopeak = int.Parse(sortInputs[42][0]);
            minatpeak = int.Parse(sortInputs[43][0]);
            maxatpeak = int.Parse(sortInputs[44][0]);
            ntsl80areas = int.Parse(sortInputs[47][0]);

            char delim = ' ';
            dataArray(acrs, 33, 0, delim); //acrs schedule
            dataArray(propdevcost, 45, 0, delim); //prop dev cost
            tslArrayFirst = new double[ntsl80areas][];
            tslArraySecond = new double[ntsl80areas][];
            tslArrayNames = new string[ntsl80areas];

            for (int i = 0; i < ntsl80areas; i++)
            {
                tslArrayFirst[i] = new double[9];  // array to hold region codes
                tslArraySecond[i] = new double[6];
                dataArray(tslArrayFirst[i], i + 49, 1, delim);   // region codes
                dataArray(tslArraySecond[i], i + 60, 1, delim);   // region codes pt 2
                tslArrayNames[i] = sortInputs[i + 49][0];
            }

        }
        private void dataArray(double[] destinationArray, int arrayReference, int secondReference, char delimiter)
        {
            char[] delimit = { delimiter };
            string[] tempString = sortInputs[arrayReference][secondReference].Split(delimit, StringSplitOptions.RemoveEmptyEntries);
            for (int i = 0; i < destinationArray.Length; i++)
                destinationArray[i] = double.Parse(tempString[i]);
        }
        private void computeInputs(int calcTSLarray)
        {                           // 2016: tslarray is region costs from textfile, added check to skip (costs entered from webform)
            if (delayyears > 0)
                if (delayyears % delayincr != 0)
                    resultsViewer53.Add("Years delay not a multiple of delay increment.");

            pplus1 = periods + 1;
            pplusdelay = pplus1 + delayyears;
            pdisnom = (1 + privdis) * (1 + inflation);
            sdisnom = (1 + socdis) * (1 + inflation);
            int matchcheck = 0;
            if (calcTSLarray == 1)
                for (int i = 0; i < ntsl80areas; i++)
                    if (region == tslArrayNames[i])
                    {
                        bk = tslArrayFirst[i][0];
                        ck = tslArrayFirst[i][1];
                        dk = tslArrayFirst[i][2];
                        alpha = tslArrayFirst[i][3];
                        beta = tslArrayFirst[i][4];
                        reffield = tslArrayFirst[i][5];
                        ftract = tslArrayFirst[i][6];
                        opcost = tslArrayFirst[i][7];
                        trancost = tslArrayFirst[i][8];
                        explcost = tslArraySecond[i][0];
                        prodlag = (int)tslArraySecond[i][1];
                        ffac1 = tslArraySecond[i][2];
                        ffac2 = tslArraySecond[i][3];
                        rsp1 = tslArraySecond[i][4];
                        rsp2 = tslArraySecond[i][5];
                        matchcheck++;
                    }
            produceyears = nyearsprop + yearstopeak + maxatpeak + maxyrsdecline + prodlag;
        }
        private void declareArray()
        {
            prices = new double[pricegrid, pplusdelay];
            pricesreal = new double[pricegrid, pplusdelay];
            priceprob = new double[pricegrid, pricegrid, pplusdelay];
            errors = new double[pricegrid];
            trendprice = new double[pplusdelay + produceyears];
            exprices = new double[pricegrid, pplusdelay, produceyears];
            expricesreal = new double[pricegrid, pplusdelay, produceyears];
            solution = new decision[pricegrid, reservegrid + 4, pplus1];
            fielddata = new double[reservegrid + 1, 2];
            probexplore = new double[pplus1, 2];
            probdevelop = new double[pplus1, 2];
            delayphold = new double[pricegrid];
            delayresults = new double[delayyears / delayincr + 1, 3];
        }
        private void calctrendprice()
        {
            //web2 note: trendprice(i) shows the price at the start of period i, 
            //which is presumed to be the same as the price at the end of period i-1
            double grow = 1 + realtrend;
            for (int i = 1; i <= (pplusdelay + produceyears); i++)
                trendprice[i - 1] = trendstart * Math.Pow(grow, i);
        }
        private void makemult(double[] outerr, int n, double sdev)
        {
            //web2 note: This creates n multiplicative error terms
            double st, st2;
            st = 1 / (double)n;
            st2 = 0.5 * st;
            for (int i = 0; i < outerr.Length; i++)
            {
                outerr[i] = invcdfn(st2) * sdev;
                st2 = st2 + st;
            }
        }
        private double invcdfn(double prob)
        {        //web2 note: Approximation to inverse cumulative normal function based on formula in Johnson and Kotz, pg. 56
            double jkalp, jkbet, c, k, t1 = 0, t2 = 0, p1 = 0, p2 = 0;
            double prob2 = 0;
            double sign = 0;
            jkalp = .644693;
            jkbet = .161984;
            c = 4.874;
            k = 6.158;

            if (prob == .5)
                return 0;
            else if (prob > 0 && prob < .5)
            {
                prob2 = prob + 2 * (.5 - prob);
                sign = -1;
            }
            else if (prob > .5 && prob < 1)
            {
                prob2 = prob - 2 * (prob - .5);
                sign = 1;
            }
            else
                resultsViewer53.Add("Error: Input to invcdfn not between 0 and 1");
            t1 = Math.Pow((1 - prob), (1 / k * -1));
            p1 = (Math.Pow((t1 - 1), (1 / c)) - jkalp) / jkbet;
            t2 = Math.Pow((1 - prob2), (1 / k * -1));
            p2 = (Math.Pow((t2 - 1), (1 / c)) - jkalp) / jkbet;

            return (sign * (Math.Abs(p1) + Math.Abs(p2)) / 2);
        }
        private void firstyearprice(double startingprice)
        {                //web2 note: The next 4 lines compute the possible prices in year 1
            double v1;
            for (int i = 0; i < pricegrid; i++)
            {
                v1 = startingprice * (Math.Pow((1 + realtrend), drift)) * Math.Pow((trendprice[0] / startingprice), pulltotrend);
                prices[i, 0] = v1 * Math.Exp(errors[i]);
            }

            /* web2 note: The following nested loop fills the probabilities for year 1. Each entry is identical and
             * equals 1/pricegrid. Also, for year 1, a 1d array would do because there is only one price to come from
             * in the previous year: start. The probabilities are recorded int he priceprob array. Each row in the
             * priceprob array is identical when the third subscript equals 1. */
            double fillprob = 1 / (double)pricegrid;
            for (int i = 0; i < pricegrid; i++)
                for (int j = 0; j < pricegrid; j++)
                    priceprob[i, j, 0] = fillprob;
        }
        private void nextyrpr(int count)
        {
            /* web2 note: This computes the prices and probabilities that are used as inputs to the dynamic optimization
             * program. pr() is the price matrix, n is the year which the price is being calculated for (ie next year),
             * pb() is the matrix of price transition probabilities.             */

            double v1;
            int t = pricegrid;
            int tsq = t * t;
            double rectsq = 1 / (double)tsq; // never used in web2, kept it in this rewrite for some reason
            double[,] fullpr = new double[tsq, 2]; // Dummy matrix to hold results
            for (int i = 1; i <= t; i++)
                for (int j = 1; j <= t; j++)
                {
                    v1 = prices[i - 1, count - 2] * Math.Pow((1 + realtrend), drift);
                    fullpr[(i - 1) * t + j - 1, 0] = v1 * Math.Pow((trendprice[count - 1] / prices[i - 1, count - 2]), pulltotrend) * Math.Exp(errors[j - 1]);
                    fullpr[(i - 1) * t + j - 1, 1] = i;
                }
            /* web2 note: fullpr() has all possible prices for next year. The first column contains the price and 
             * the second column is the price grid level where that price came from in the previous year. 
             * 
             * This next loop divides the prices into t equal percentile groups. The mean price in each group is calculated
             * and the transition probabilities of going from one pricegrid level to the next are written to the pb() matrix. */

            sortMatrix(tsq, fullpr);

            /* web2 note: k accumulates for percentile average
             * index is where the grid level where the price came from, i is the grid level where it is going.
             * thus, pb(index,i,n) will show the probability of going from price grid level index in year n-1 to
             * price grid i in year n. pb = priceprob, probs records matrix of probabilities
             * k is the average price for the ith grid level */

            double k;
            int index;

            for (int i = 1; i <= t; i++)
            {
                k = 0;
                for (int j = 1; j <= t; j++)
                {
                    k = k + fullpr[(i - 1) * t + j - 1, 0];
                    index = (int)fullpr[(i - 1) * t + j - 1, 1];
                    priceprob[index - 1, i - 1, count - 1] = priceprob[index - 1, i - 1, count - 1] + (1 / (double)t);
                }
                k = k / (double)t;
                prices[i - 1, count - 1] = k;
            }
        }
        private void sortMatrix(int tsq, double[,] fullpr)
        {
            double temp;
            for (int i = 0; i < tsq; i++)
                for (int j = tsq - 1; j > i; j--)
                    if (fullpr[j, 0] < fullpr[j - 1, 0])
                    {
                        temp = fullpr[j, 0];
                        fullpr[j, 0] = fullpr[j - 1, 0];
                        fullpr[j - 1, 0] = temp;
                        temp = fullpr[j, 1];
                        fullpr[j, 1] = fullpr[j - 1, 1];
                        fullpr[j - 1, 1] = temp;
                    }
        }
        private void expectedprice()
        {
            // currently set to always calculate prices, aka removed option to import prices. 
            // import price function originally only used to save processing time, irrelevant issue now.
            // updated web2 will allow for custom price vectors, but that can't be done here.

            double[] futureprices = new double[produceyears];
            ArrayList priceslist = new ArrayList();
            for (int i = 1; i <= pplusdelay; i++)
            {
                for (int j = 1; j <= pricegrid; j++)
                {
                    averageprice(j, i, futureprices);
                    for (int k = 0; k < produceyears; k++)
                        exprices[j - 1, i - 1, k] = futureprices[k];
                }
            }
            priceslist.Add(title);
            for (int i = 0; i < pplusdelay; i++)
                for (int j = 0; j < pricegrid; j++)
                    for (int k = 0; k < produceyears; k++)
                        priceslist.Add(exprices[j, i, k].ToString("N5"));
            string[] printArray = priceslist.ToArray(typeof(string)) as string[];
            //writeOutput(printArray, currDirectory + "\\" + pricefile, 0, 0);
        }
        private void averageprice(int grid, int tindex, double[] futprices)
        {
            int gridsq = pricegrid * pricegrid;
            double avp1, avp2;
            double[,] hold = new double[pricegrid, produceyears];
            double[] fullpr = new double[gridsq];
            for (int i = 0; i < pricegrid; i++)
                hold[i, 0] = prices[grid - 1, tindex - 1];
            for (int i = 2; i <= produceyears; i++)
            {
                int v = tindex + i - 1;
                for (int j = 1; j <= pricegrid; j++)
                    for (int k = 1; k <= pricegrid; k++)
                    {
                        avp1 = Math.Pow((trendprice[v - 1] / hold[j - 1, i - 2]), pulltotrend);
                        avp2 = Math.Pow((1 + realtrend), drift);
                        fullpr[(j - 1) * pricegrid + k - 1] = hold[j - 1, i - 2] * avp1 * avp2 * Math.Exp(errors[k - 1]);
                    }
                Array.Sort(fullpr);
                for (int r = 1; r <= pricegrid; r++)
                {
                    double u = 0;
                    for (int s = 1; s <= pricegrid; s++)
                        u = u + fullpr[(r - 1) * pricegrid + s - 1];
                    u = u / (double)pricegrid;
                    hold[r - 1, i - 1] = u;
                    // web2 note: hold[] now has the future price matrix given grid, tindex
                }
            }
            for (int y = 0; y < produceyears; y++)
            {
                double temp = 0;
                for (int z = 0; z < pricegrid; z++)
                    temp = temp + hold[z, y];
                temp = temp / (double)pricegrid;
                futprices[y] = temp;
            }
        }
        private void makereal()
        {
            expricesreal = (double[, ,])exprices.Clone();
            pricesreal = (double[,])prices.Clone();
            for (int i = 1; i <= pplusdelay; i++)
                for (int j = 1; j <= pricegrid; j++)
                {
                    prices[j - 1, i - 1] = prices[j - 1, i - 1] * Math.Pow((1 + inflation), (double)i);
                    for (int k = 1; k <= produceyears; k++)
                        exprices[j - 1, i - 1, k - 1] = exprices[j - 1, i - 1, k - 1] * Math.Pow((1 + inflation), (i + k - 1));
                }
        }
        private void makefield()
        {
            // web2: This subprogram generates the possible field sizes for the reserve grid (1st column of fielddata)
            // The probabilities are in the second column, the last row of fielddata is the dry outcome case.
            double[] ehold = new double[reservegrid];
            double v1 = fieldsd * fieldsd;
            double v2, correction;
            double tempsize = 0;
            if (fieldsd > 0)
                v2 = Math.Sqrt(Math.Log(fieldsize * fieldsize + v1) - 2 * (Math.Log(fieldsize)));
            else
                v2 = 0;
            double v3 = Math.Log(fieldsize) - .5 * v2 * v2;
            double p1 = 1 / (double)reservegrid * probsucc;
            makemult(ehold, reservegrid, v2);
            for (int i = 0; i < reservegrid; i++)
            {
                fielddata[i, 0] = Math.Exp(v3 + ehold[i]);
                fielddata[i, 1] = p1;
                tempsize = tempsize + fielddata[i, 0];
            }
            correction = fieldsize / (tempsize / (double)reservegrid);
            for (int i = 0; i < reservegrid; i++)
                fielddata[i, 0] = correction * fielddata[i, 0];
            fielddata[reservegrid, 0] = 0;              //qbasic is reservegrid+1
            fielddata[reservegrid, 1] = 1 - probsucc;   //qbasic is reservegrid+1
        }
        private void optimizeback(decision[, ,] solution)
        {
            for (int i = pplus1; i >= 1; i--)
            {
                for (int pindex = 1; pindex <= pricegrid; pindex++)
                    for (int rindex = 1; rindex <= reservegrid + 4; rindex++)
                        best(pindex, rindex, i, solution);
            }
        }
        private void develop(double[] pricepath, int state, int when, ref double devATNPV, ref double devNEV, int yap)
        {
            /* web2 note: pricepath() = nominal price path beginning at first year of development
             * state = state of lease, see subprogram sit
             * when = years since lease issuance when lease is issued when=0
             * ATNPV = output of ATNPV of developing
             * NEV = output of NEV of developing
             * YearbyYear() = matrix for writing production and royalties to, not used now note: removed
             * yap = years at peak production */

            int year1stprod = 0;
            double amount = 0, ppacum = 0, totnomplatcost = 0;
            int v1 = pricepath.Length;
            double[,] temp = new double[v1, 10]; // pricepath.length = 24
            if (state < 1 || state > reservegrid)
                resultsViewer53.Add("Error in subprogram develop, state is out of range");
            else
                amount = fielddata[state - 1, 0];
            double peakflow = capacity(amount, yap);
            double platformcost = calcdevcost(amount, peakflow / amount);
            double fixedcost = fixdollars(peakflow, ffac1, ffac2); // cost per barrel in base year dollars

            for (int i = 1; i <= nyearsprop; i++) // int i = 1 because qbasic code uses i in exponent calculation
            {                                                   // col 1 has development costs
                ppacum = propdevcost[i - 1] + ppacum;               // proportion of costs bourne
                temp[i - 1, 0] = propdevcost[i - 1] * platformcost * Math.Pow((1 + inflation), (i - 1 + when)); // qbasic (i-1+when)
                totnomplatcost = totnomplatcost + temp[i - 1, 0];   // nominal platform cost
                year1stprod = i + 1 + prodlag;
                if (ppacum >= 1)
                    break;                                      // platform is finished, i is finishyear+1
            }
            // web2: GOSUB production line 643, figure production in col 2 fixed cost in col 5

            int yrsprd = yearstopeak + yap + maxyrsdecline;
            int z;
            double pv1 = peakflow / (double)(yearstopeak + 1);
            double massbalcheck = 0;
            for (int i = year1stprod; i <= yrsprd + year1stprod - 1; i++) //qbasic yrsprd+yr1st-1
            {
                z = i + 1 - year1stprod;    // time relative to start of production 1=start
                if (z <= yearstopeak)       // building production
                    temp[i - 1, 1] = z * pv1;
                else if (z > yearstopeak && z <= yearstopeak + yap) // peak production
                    temp[i - 1, 1] = peakflow;
                else if (z == yearstopeak + yap + 1) //first year of decline
                    temp[i - 1, 1] = (1 - declinerate / 2) * peakflow;
                else                        // standard exponential decline
                    temp[i - 1, 1] = temp[i - 2, 1] * (1 - declinerate);
                temp[i - 1, 4] = fixedcost * Math.Pow((1 + inflation), (i - 1 + when));
                massbalcheck = massbalcheck + temp[i - 1, 1];
            }
            if (Math.Abs(massbalcheck - amount) > 10000) //wrong amount of oil produced
                resultsViewer53.Add("Error Mass Balance Check, subprogram best");

            // web2: GOSUB transportation line 674
            for (int i = 1; i <= v1; i++)
            {
                temp[i - 1, 2] = trancost * Math.Pow((1 + inflation), (when + i - 1)); // nominal transcost
                temp[i - 1, 3] = pricepath[i - 1] - temp[i - 1, 2]; // nominal wellhead cost
                temp[i - 1, 5] = temp[i - 1, 1] * opcost * Math.Pow((1 + inflation), (when + i - 1)); // nom opcost
            }

            // web2: GOSUB depreciation line 681
            int acrsindex = -1;     // this is probably bad coding logic, but converting from qbasic
            for (int i = year1stprod; i <= year1stprod + nacrsyrs - 1; i++)
            {
                acrsindex++;
                temp[i - 1, 8] = totnomplatcost * tangportion * acrs[acrsindex];
            }

            // web2: GOSUB financial line 687
            double grossrevenue, bonusdepletion, intangible, taxincome; // grossincome = revenue at wellhead
            // taxincome is not exactly taxable income, correction for depreciation and depletion a few lines after
            for (int i = 0; i < v1; i++)
            {
                grossrevenue = temp[i, 1] * temp[i, 3];
                bonusdepletion = (temp[i, 1] / amount) * leasecost;
                temp[i, 6] = grossrevenue - temp[i, 0] - temp[i, 4] - temp[i, 5]; // NEV column
                temp[i, 7] = grossrevenue * royalty; // col 7 is royalty
                intangible = temp[i, 0] * (1 - tangportion);
                taxincome = grossrevenue - temp[i, 5] - temp[i, 4] - temp[i, 7] - intangible;
                temp[i, 9] = taxincome * (1 - tax) + tax * (temp[i, 8] + bonusdepletion) - tangportion * temp[i, 0];
                // temp[i,9] holds ATNPV column = aftertaxincome + depreciation - tangible costs
            }

            // web2: GOSUB discountit line 700
            int firstshutyear = year1stprod + yearstopeak;
            double socdollars = 0, pridollars = 0;
            for (int i = 1; i <= v1; i++)
            {
                if (i < firstshutyear || temp[i - 1, 9] >= 0) // keep producing
                {
                    socdollars = socdollars + temp[i - 1, 6] / (Math.Pow(sdisnom, (i - 1 + distime)));
                    pridollars = pridollars + temp[i - 1, 9] / (Math.Pow(pdisnom, (i - 1 + distime)));
                }
                else
                    break; // shutting operation in
            }
            devNEV = socdollars;
            devATNPV = pridollars;
        }
        private double fixdollars(double pf, double ff1, double ff2)
        {
            return (ff1 + ff2 * pf); // linear fixed cost function
        }
        private double calcdevcost(double barrels, double howhigh)
        {
            double cdc1 = alpha * Math.Pow((barrels / reffield), (beta + bk - 1)) * Math.Pow(ftract, (1 - beta));
            double cdc2 = barrels * howhigh; // peak production in barrels
            return (cdc2 * (cdc1 * Math.Pow(cdc2, bk * -1) + ck) + dk);
        }
        private double capacity(double resourcesize, int timeatpeak)
        {
            // web2note: Computed the installed capacity as a function of resource size and the years at peak
            double v1 = 1 / (double)(yearstopeak + 1);
            double v2 = 0;
            for (int i = 1; i <= yearstopeak; i++)
                v2 = v2 + i;
            double v3 = 1 - declinerate;
            double v4 = (1 + v3) / 2;
            double v5 = ((1 - Math.Pow(v3, maxyrsdecline)) / (1 - v3));
            double v6 = v1 * v2 + timeatpeak + v4 * v5;
            return (resourcesize / v6);
        }
        private void explore(int pricestep, int state, int when, ref double exATNPV, ref double exNEV, decision[, ,] matrix)
        {
            if (state != reservegrid + 2)
                resultsViewer53.Add("Error in explore. state != reservegrid+2");

            double prob1, prob2, v1, v2;
            double vprivate = 0, vsocial = 0;
            // web2 note: this next set of nest loops sum over all combinations of price and exploration
            // outcome. The i variable covers exploration outcomes, i=1 means small find, i=reservegrid+1
            // means the lease was dry, see subprogram sit

            for (int i = 0; i < reservegrid + 1; i++)
                for (int j = 0; j < pricegrid; j++)
                {
                    prob1 = priceprob[pricestep - 1, j, when]; // qbasic when+1
                    prob2 = fielddata[i, 1];
                    v1 = matrix[j, i, when].privvalue;
                    v2 = matrix[j, i, when].socialval;
                    vprivate = vprivate + prob1 * prob2 * v1;
                    vsocial = vsocial + prob1 * prob2 * v2;
                }
            double ecost = explcost * Math.Pow((1 + inflation), when);
            double tfacsoc = Math.Pow((1 / sdisnom), distime);
            double tfacpri = Math.Pow((1 / pdisnom), distime);
            exNEV = (1 / sdisnom) * vsocial - ecost * tfacsoc;
            exATNPV = (1 / pdisnom) * vprivate - (1 - tax) * (ecost + rental) * tfacpri;
        }
        private void abandon(ref double abATNPV, ref double abNEV)
        {
            double tfacpri = Math.Pow((1 / pdisnom), distime); // discount timing factor
            abATNPV = leasecost * tax * tfacpri;
            abNEV = 0;
        }
        private void withrent(ref double wrATNPV, ref double wrNEV, double vprivate, double vsocial)
        {
            double tfac = Math.Pow(1 / pdisnom, distime); // discount timing factor
            wrATNPV = (1 / pdisnom) * vprivate - (1 - tax) * rental * tfac;
            wrNEV = (1 / sdisnom) * vsocial;
        }
        private void norent(ref double atnpvz, ref double nevz, double vprivatez, double vsocialz)
        {
            atnpvz = (1 / pdisnom) * vprivatez;
            nevz = (1 / sdisnom) * vsocialz;
        }
        private void sit(int pricestep, int state, int when, ref double ATNPV, ref double NEV, decision[, ,] solutionMatrix)
        {
            /* web2 note: pricestep = pricestep grid level, state = existing condition of the lease. State is a tricky
             * variable, it is an integer that can range from 1 to reservegrid + 4 as follows:
             * state = 1 means lease is explored and smallest reservegrid was found, 2 = explored, next size found,
             * state = reservegrid means explored and largest size field found, state = rgrid+1 means explored and dry,
             * state = rgrid+2 means unexplored lease, state = rgrid+3 means abandoned lease, state = rgrid+4 means 
             * developed lease. when = years since lease was issued, ATNPV and NEV are output variables */
            if (state >= reservegrid + 3 || state < 1)
                resultsViewer53.Add("error in sit, should not reach this point.");

            // web2 note: Only unexplored and explored but undeveloped leases make it here
            double prob1, v1, v2;
            double vprivate = 0, vsocial = 0;
            for (int i = 0; i < pricegrid; i++)
            {
                prob1 = priceprob[pricestep - 1, i, when]; // qbasic when +1
                // prob1 is probability of going from pricegrid level pricestep when year from lease issuance to
                // price grid level i in the next year
                v1 = solutionMatrix[i, state - 1, when].privvalue; // private value this state next year
                v2 = solutionMatrix[i, state - 1, when].socialval; // social value given this state next year
                vprivate = vprivate + prob1 * v1;
                vsocial = vsocial + prob1 * v2;
            }
            // charge rentals depends on value of exploration/development constraints. constraints binding = no rentals

            if (state == reservegrid + 2 && when >= yrfirstexpl) // unexplored lease which can be explored
                withrent(ref ATNPV, ref NEV, vprivate, vsocial);
            else if (state == reservegrid + 2 && when < yrfirstexpl) // no rent
                norent(ref ATNPV, ref NEV, vprivate, vsocial);
            else if (when >= yrtodev)
                withrent(ref ATNPV, ref NEV, vprivate, vsocial);
            else if (when < yrtodev)
                norent(ref ATNPV, ref NEV, vprivate, vsocial);
            else
                resultsViewer53.Add("Error in SIT");
        }
        public decision actionMatrix(double priv, double soc, string dec)
        {
            decision exploreChoice = new decision();
            exploreChoice.privvalue = priv;
            exploreChoice.socialval = soc;
            exploreChoice.action = dec;
            return exploreChoice;
        }
        private void best(int pricestep, int state, int when, decision[, ,] outmat)
        {
            double[] pricevec = new double[produceyears];
            double checkpv = 0, checksc = 0;
            int imat = (pricestep - 1);
            int jmat = (state - 1);
            int kmat = (when - 1);
            for (int i = 0; i < produceyears; i++)
                pricevec[i] = exprices[pricestep - 1, when - 1, i];
            double dp = 0, ds = 0, abap = 0, abas = 0, sp = 0, ss = 0, ep = 0, es = 0;
            if (state >= 1 && state <= reservegrid) // lease is explored and oil found
            {
                // web2 note: This code tries different years at peak and stores the best option as the value of
                // developing testing is done only if deveopment is feasible (when>=yrtodev)
                if (when > yrtodev)
                {
                    for (int yapcc = minatpeak; yapcc <= maxatpeak; yapcc++)
                    {
                        develop(pricevec, state, when, ref dp, ref ds, yapcc);
                        if (yapcc == minatpeak || dp > checkpv)//store values as base case or check if other peak is better
                        {   // note: changed some of the qbasic logic here
                            checkpv = dp;
                            checksc = ds;
                            besttimetop = yapcc; //store # of years at peak or store best option
                        }
                    }
                    dp = checkpv;
                    ds = checksc;
                } // if development not feasible, dp = ds = 0
                abandon(ref abap, ref abas);
                if (when < periods)
                    sit(pricestep, state, when, ref sp, ref ss, outmat);
                // need to find best option for explored leases with oil
                //Console.WriteLine("when yrtodev periods " + when + " " + yrtodev + " " + periods);
                //Console.WriteLine("abap abas dp ds " + abap + " " + abas + " " + dp + " " + ds);
                //Console.WriteLine("sp ss ep es " + sp + " " + ss + " " + ep + " " + es);
                if (when >= yrtodev && when < periods)  // develop aban or wait are options
                {
                    if (dp >= abap && dp >= sp)         //develop is best
                        outmat[imat, jmat, kmat] = actionMatrix(dp, ds, "D");
                    else if (sp > dp && sp >= abap)     //wait is best
                        outmat[imat, jmat, kmat] = actionMatrix(sp, ss, "W");
                    else if (abap > dp && abap > sp)    //abandon is best
                        outmat[imat, jmat, kmat] = actionMatrix(abap, abas, "A");
                    else
                        resultsViewer53.Add("Error in best subprogram: 1");
                }
                else if (when >= yrtodev && when >= periods) // aban or develop are options
                {
                    if (dp >= abap)                     //development best
                        outmat[imat, jmat, kmat] = actionMatrix(dp, ds, "D");
                    else if (abap > dp)                 // abandon best
                        outmat[imat, jmat, kmat] = actionMatrix(abap, abas, "A");
                    else
                        resultsViewer53.Add("Error in subprogram best, hhh");
                }
                else if (when < yrtodev && when < periods) // aban or wait are options
                {
                    if (sp >= abap)                     // wait
                        outmat[imat, jmat, kmat] = actionMatrix(sp, ss, "W");
                    else if (sp < abap)                 // abandon
                        outmat[imat, jmat, kmat] = actionMatrix(abap, abas, "A");
                    else
                        resultsViewer53.Add("Error in subprogram best, ggg");
                }
                else if (when < yrtodev && when >= periods) // abandon is only option
                    outmat[imat, jmat, kmat] = actionMatrix(abap, abas, "A");
                else
                    resultsViewer53.Add("Error in subprogram best, flow shouldn't reach here.");
            }
            else if (state == reservegrid + 1) //lease is explored and dry
            {
                if (when <= explterm + 1) // abandon lease
                {
                    abandon(ref abap, ref abas);
                    outmat[imat, jmat, kmat] = actionMatrix(abap, abas, "A");
                }
                else // lease already should have been abanadoned, no action possible
                    outmat[imat, jmat, kmat] = actionMatrix(0, 0, "N");
            }
            else if (state == reservegrid + 2) // lease is unexplored
            {
                abandon(ref abap, ref abas);
                if (when < explterm)       // waiting is option
                    sit(pricestep, state, when, ref sp, ref ss, outmat);
                if (when <= explterm && when >= yrfirstexpl)   // explore is option
                    explore(pricestep, state, when, ref ep, ref es, outmat);
                if (when < explterm && when >= yrfirstexpl)        // explore wait or abandon are option
                {
                    if (ep >= abap && ep >= sp) // explore is best
                        outmat[imat, jmat, kmat] = actionMatrix(ep, es, "E");
                    else if (sp > ep && sp >= abap) // wait is best
                        outmat[imat, jmat, kmat] = actionMatrix(sp, ss, "W");
                    else if (abap > ep && abap > sp) // abandon is best
                        outmat[imat, jmat, kmat] = actionMatrix(abap, abas, "A");
                    else
                        resultsViewer53.Add("Error in subprogram best:2");
                }
                else if (when < explterm && when < yrfirstexpl) // wait or abandon are options
                {
                    if (sp >= abap) //wait is best
                        outmat[imat, jmat, kmat] = actionMatrix(sp, ss, "W");
                    else if (abap > sp) // abandon is best
                        outmat[imat, jmat, kmat] = actionMatrix(abap, abas, "A");
                    else
                        resultsViewer53.Add("Error in subprogram best: 4");
                }
                else if (when == explterm && when >= yrfirstexpl) // explore or abandon
                {
                    if (ep >= abap) // explore is best
                        outmat[imat, jmat, kmat] = actionMatrix(ep, es, "E");
                    else if (abap > ep) // abandon is best
                        outmat[imat, jmat, kmat] = actionMatrix(abap, abas, "A");
                    else
                        resultsViewer53.Add("Error in subprogram best: 22");
                }
                else if (when == explterm && when < yrfirstexpl) // abandon is only option left
                    outmat[imat, jmat, kmat] = actionMatrix(abap, abas, "A");
                else if (when > explterm) // lease expired, no action possible
                    outmat[imat, jmat, kmat] = actionMatrix(0, 0, "N");
                else
                    resultsViewer53.Add("Error in subprogram best, 111");
            }
            else if (state == reservegrid + 3 || state == reservegrid + 4) // lease abandoned or developed
                outmat[imat, jmat, kmat] = actionMatrix(0, 0, "N");
            else
                resultsViewer53.Add("Error in subprogram best, pricestep: " + pricestep + " state: " + state + " when: " + when);

            /* debug Console.WriteLine("Best: pricestep: " + pricestep + " state: " + state + " when: " + when
                 + "\n dp: " + dp + " ds: " + ds + " abap: " + abap + " abas: " + abas
                 + "\n sp: " + sp + " ss: " + ss + " ep: " + ep + " es: " + es
                 + "\n action: " + outmat[imat, jmat, kmat].action); */
        }
        public decision optimizestart(decision[, ,] solutionMatrix)
        {
            int pindex = 1;
            decision choice = new decision();
            double abap = 0, abas = 0, sp = 0, ss = 0, ep = 0, es = 0;
            abandon(ref abap, ref abas);
            explore(pindex, reservegrid + 2, 0, ref ep, ref es, solutionMatrix);
            if (explterm >= 1) // see if waiting is worthwhile
                sit(pindex, reservegrid + 2, 0, ref sp, ref ss, solutionMatrix);
            if (yrfirstexpl == 0 && explterm >= 1) //exploring, wait, abandon is possible
            {
                if (ep >= sp && ep >= abap)         // exploring is best
                    choice = actionMatrix(ep, es, "E");
                else if (sp > ep && sp >= abap)     // waiting is best
                    choice = actionMatrix(sp, ss, "W");
                else if (abap > sp && abap > ep)    // abandon is best
                    choice = actionMatrix(abap, abas, "A");
                else                                // you screwed up
                    resultsViewer53.Add("Error in optimizestart");
            }
            else if (yrfirstexpl > 0 && explterm >= 1)
            {
                if (sp >= abap) // waiting is best
                    choice = actionMatrix(sp, ss, "W");
                else if (abap > sp) // abandon is best
                    choice = actionMatrix(abap, abas, "A");
                else
                    resultsViewer53.Add("Error in optimizestart1");
            }
            else if (yrfirstexpl == 0 && explterm == 0)
            {
                if (ep >= abap)     // explore is best
                    choice = actionMatrix(ep, es, "E");
                else                // abanadon is best
                    choice = actionMatrix(abap, abas, "A");
            }
            else if (yrfirstexpl > 0 && explterm == 0)  // must abandon
                choice = actionMatrix(abap, abas, "A");
            else
                resultsViewer53.Add("ERROR in OPTIMIZESTART");
            // for Sarah 10/17/2012
            aDecision.action = "A";
            wDecision.action = "W";
            eDecision.action = "E";
            aDecision.privvalue = abap;
            aDecision.socialval = abas;
            wDecision.privvalue = sp;
            wDecision.socialval = ss;
            eDecision.privvalue = ep;
            eDecision.socialval = es;
            return choice;
        }
        private void probexploration()
        {
            double[,] dmatrix = new double[pricegrid, explterm];
            double[,] oddshold = new double[pricegrid, explterm];
            double[,] abanmatrix = new double[pricegrid, explterm];
            double v1 = 0, v2 = 0, v3 = 0;
            double s1, s2, s3;
            int ustate = reservegrid + 2; // sets state as unexplored in solution matrix, set subprog "SIT"
            // qbasic reservegrid+2
            for (int j = 0; j < explterm; j++)
                for (int i = 0; i < pricegrid; i++)
                {
                    if (solution[i, ustate - 1, j].action == "E")
                        dmatrix[i, j] = 0;      // 0 means explored is optimal action, given i,j
                    else if (solution[i, ustate - 1, j].action == "A")
                        dmatrix[i, j] = 0;      // aban is optimal
                    else
                        dmatrix[i, j] = 1;      // expl/aban not optimal
                }

            for (int j = 0; j < explterm; j++)
                for (int i = 0; i < pricegrid; i++)
                    for (int k = 0; k < pricegrid; k++)
                    {
                        if (j == 0 && explDecision.action != "W")   // tract was explored or abandoned at time 0
                            oddshold[i, j] = 0;
                        else if (j == 0 && explDecision.action == "W")  // waiting is optimal
                            oddshold[i, j] = priceprob[k, i, j] * (1 / (double)pricegrid) + oddshold[i, j];
                        else if (j > 0)    // year 2 or beyond
                        {
                            v1 = priceprob[k, i, j]; // prob of being at price level i in year j given price k in year j-1
                            v2 = dmatrix[k, j - 1];  // v2 is 1 if price level k not expl/aban in year j-1
                            v3 = oddshold[k, j - 1]; // prob of being at k in year j-1 with no prior expl/aban decision
                            oddshold[i, j] = v1 * v2 * v3 + oddshold[i, j];
                        }
                    }
            for (int j = 0; j < explterm; j++)
                for (int i = 0; i < pricegrid; i++)
                {                                   // abanmatrix is a 0/1 matrix to check for abandonment
                    if (solution[i, ustate - 1, j].action == "A")
                        abanmatrix[i, j] = 0;       // aban is optimal
                    else
                        abanmatrix[i, j] = 1;       // aban not optimal
                }

            for (int j = 0; j < explterm; j++)
            {
                for (int i = 0; i < pricegrid; i++)
                {
                    s1 = Math.Abs(dmatrix[i, j] - 1);   // s1 = 1 if expl/aban, 0 otherwise
                    s2 = abanmatrix[i, j];                // s2 = 0 if aban, 1 otherwise
                    s3 = s1 * s2;                         // s3 = 1 if explore is optimal and aban is not optimal, 0 otherwise
                    probexplore[j, 0] = oddshold[i, j] * s3 + probexplore[j, 0];
                }
                if (j == 0 && explDecision.action == "W")
                    probexplore[j, 1] = probexplore[j, 0];
                else if (j == 0 && explDecision.action == "E")
                    probexplore[j, 1] = 1;
                else if (j == 0 && explDecision.action == "A")
                {
                    probexplore[j, 0] = 0;
                    probexplore[j, 1] = 0;
                }
                else
                    probexplore[j, 1] = probexplore[j - 1, 1] + probexplore[j, 0];
            }

            // web2 note: probexplore sums oddshold for exploration decisions only. the first column of probexplore
            // contains marginal probabilities and the second contains cumulative probabilities. there is some logic
            // checking depending on the decision of in year 0.

        }
        private void certainty(ArrayList list)
        {   // prints value of developing a known field size with known prices
            int tempperiods = periods, tempexplterm = explterm, tempfirstyrexpl = yrfirstexpl, tempyrtodev = yrtodev, temppplus1 = pplus1;
            
            double[] cprices = new double[produceyears];
            cprices[0] = startprice;
            double cdp = 0, cds = 0, checkpv = 0, checksc = 0;

            for (int i = 1; i <= produceyears - 1; i++)     // i = 1 because of qbasic variable in calculation
                cprices[i] = trendprice[i - 1] * Math.Pow((1 + inflation), i); // qbasic is cprice[i-1], trendprice[i]
            double storeit = fielddata[0, 0];   // store correct fieldsize
            fielddata[0, 0] = fieldsize;        // insert expected fieldsize in fielddata. done bc structure of develop
            for (int yapcc = minatpeak; yapcc <= maxatpeak; yapcc++)
            {
                develop(cprices, 1, 0, ref cdp, ref cds, yapcc);
                if (yapcc == minatpeak || cdp > checkpv)
                {
                    checkpv = cdp;
                    checksc = cds;
                    besttimetop = yapcc;
                }
            }
            certaintyprint(list, cdp, cds);
            fielddata[0, 0] = storeit;

            if (delayyears != 0)
            {
                delayanalysis();
                periods = tempperiods;
                explterm = tempexplterm;
                yrfirstexpl = tempfirstyrexpl;
                yrtodev = tempyrtodev;
                pplus1 = temppplus1;
                printdelay(list);
            }
        }
        private void certaintyprint(ArrayList list, double dp, double ds)
        {
            list.Add("\nThe value of developing an already discovered " + fieldsize + " field in " +
                year + " using a starting price of " + startprice + " and nominal trend prices thereafter is as follows(" +
                year + " dollars).\n\n ATNPV " + dp + "  NEV " + ds + "\nYears at peak production: " + besttimetop);
        }
        private void delayanalysis()
        {
            decision[, ,] solutionDelay;
            double[,] probexploreCopy;
            decision delayChoice;
            solutionDelay = solution;
            probexploreCopy = probexplore;
            delayresults[0, 0] = 0;
            delayresults[0, 1] = explDecision.privvalue;
            delayresults[0, 2] = explDecision.socialval;

            int count = 0;
            double pvhold, svhold;
            for (int dy = delayincr; dy <= delayyears; dy += delayincr)
            {
                periods += delayincr;
                explterm += delayincr;
                yrfirstexpl += delayincr;
                yrtodev += delayincr;
                pplus1 += delayincr;
                pvhold = 0;
                svhold = 0;
                solutionDelay = new decision[pricegrid, reservegrid + 4, pplus1];
                probexploreCopy = new double[pplus1, 2];
                optimizeback(solutionDelay);
                delayChoice = optimizestart(solutionDelay);
                delaycalc(dy, ref pvhold, ref svhold, solutionDelay);
                count++;
                delayresults[count, 0] = dy;
                delayresults[count, 1] = pvhold;
                delayresults[count, 2] = svhold;
                //Console.WriteLine("pv: " + pvhold + " sv: " + svhold);
            }
        }
        private void delaycalc(int offset, ref double pvsum, ref double svsum, decision[, ,] solutionDelay)
        {
            double pv, sv;
            for (int i = 0; i < pricegrid; i++)
            {
                pv = solutionDelay[i, reservegrid + 1, offset - 1].privvalue;
                sv = solutionDelay[i, reservegrid + 1, offset - 1].socialval;
                pvsum = pvsum + pv * (1 / (double)pricegrid);
                svsum = svsum + sv * (1 / (double)pricegrid);
            }
            pvsum = pvsum / Math.Pow(pdisnom, offset);
            svsum = svsum / Math.Pow(sdisnom, offset);
        }
        private void printsolution(int hurdleCheck, int printFinal)
        {
            ArrayList list = new ArrayList();
            printHalfSolution(list);
            if (printsize == 2)
                printFullSolution(list);
            certainty(list);       // calculates value of developing tract at fixed prices
            if (hurdleCheck == 1)
                writeHurdleResults(list);
            string[] printArray = list.ToArray(typeof(string)) as string[];
            outputFileName = currDirectory + "\\" + resultsfile;
            writeOutput(printArray, outputFileName, viewer, printFinal);
        }
        private void printHalfSolution(ArrayList list)
        {
            string dummy = "";
            string space = " ";
            list.Add("WEB2 originally programmed by Donald H. Rosenthal, USDI");
            list.Add("WEB3 reprogrammed by Kevin Nguyen, BOEM Economics");
            list.Add(title);

            list.Add("************** Program Control Inputs **************");
            list.Add(year + "\t\t\tCurrent Year");
            list.Add(region + "\t\t\tSelected Region");
            list.Add(pricegrid + "\t\t\tNumber of Price Levels");
            list.Add(reservegrid + "\t\t\tNumber of Reserve Grids");
            list.Add(delayyears + "\t\t\tNumber of Years Delay Analysis, 0 = no delay analysis");
            list.Add(delayincr + "\t\t\tYearly Increment for Delay Analysis");

            list.Add("************** Lease Term Inputs **************");
            list.Add(periods + "\t\t\tPrimary Term of Lease");
            list.Add(explterm + "\t\t\tExploration Diligence Term");
            list.Add(royalty + "\t\t\tRoyalty Rate");
            list.Add(leasecost + "\t\t\tLease Acquisition Cost");
            list.Add(rental + "\t\t\tYearly Rental Rate for Lease");
            list.Add(yrfirstexpl + "\t\t\tFirst Year Exploration Possible, 0 = no contraints");
            list.Add(yrtodev + "\t\t\tFirst Year Development Possible, 0 = no constraints");

            list.Add("********** Financial Inputs **********");
            list.Add(privdis + "\t\t\tReal Private Discount Rate");
            list.Add(socdis + "\t\t\tReal Social Discount Rate");
            list.Add(distime + "\t\t\tDiscount Timing, 0 = start of year, 1 = end of year");
            list.Add(inflation + "\t\t\tInflation Rate");
            list.Add(startprice + "\t\t\tStarting Price of Oil");
            list.Add(trendstart + "\t\t\tTrend Starting Price");
            list.Add(sigma + "\t\t\tStandard Deviation of Real Oil Prices");
            list.Add(realtrend + "\t\t\tReal Trend in Oil Prices");
            list.Add(pulltotrend + "\t\t\tPull to Trend Parameter for Price Model");
            list.Add(drift + "\t\t\tDrift Factor for Prices, 0 = no drift");
            list.Add(tangportion + "\t\t\tTangible Porition of Development Cost");
            list.Add(tax + "\t\t\tTax Rate on Profits");
            list.Add("From Regional Cost Section:");
            list.Add(explcost + "\t\tExploration Cost");
            list.Add(trancost + "\t\t\tTransportation Cost");
            list.Add(opcost + "\t\t\tOperating Cost per Barrel");

            list.Add(acrs[0] + space + acrs[1] + space + acrs[2] + space + acrs[3]);
            list.Add(acrs[4] + space + acrs[5] + space + acrs[6] + space + acrs[7] + "\tACRS Schedule");

            list.Add("********** Geological & Production Inputs **********");
            list.Add(fieldsd + "\t\tStandard Deviation of Field Size");
            list.Add(fieldsize + "\t\tExpected Field Size");
            list.Add(probsucc + "\t\t\tProbability of Finding Oil");
            list.Add(declinerate + "\t\t\tExponential Decline Rate");
            list.Add(maxyrsdecline + "\t\t\tMax Years in Decline");
            list.Add(yearstopeak + "\t\t\tYears to Peak");
            list.Add(minatpeak + "\t\t\tMin Years at Peak");
            list.Add(maxatpeak + "\t\t\tMax Years at Peak");
            list.Add(prodlag + "\t\t\tYears Delay After Platform Finished That Production Begins, 0 = no delay");

            list.Add(propdevcost[0] + space + propdevcost[1] + space + propdevcost[2] + space + propdevcost[3] + space + propdevcost[4] + space + "\t\tProportion of Development Costs Years 1-5");

            list.Add("Regional Costs: " + region);
            list.Add(bk + "\t\t\tBK");
            list.Add(ck + "\t\t\tCK");
            list.Add(dk + "\t\tDK");
            list.Add(ftract + "\t\t\tFTRACT");
            list.Add(opcost + "\t\t\tOPCOST");
            list.Add(alpha + "\t\t\tALPHA");
            list.Add(beta + "\t\t\tBETA");
            list.Add(trancost + "\t\t\tTRANCOST");
            list.Add(ffac1 + "\t\t\tFFAC1");
            list.Add(ffac2 + "\t\t\tFFAC2");
            list.Add(reffield + "\t\t\tREFFIELD");
            list.Add(explcost + "\t\tEXPLCOST");
            list.Add(prodlag + "\t\t\tPRODLAG");
            list.Add(rsp1 + "\t\t\tRSP1");
            list.Add(rsp2 + "\t\t\tRSP2");
            
            list.Add("\n\n************** Reporting Results: **************\"\n\nPossible nomimal prices during the primary term (+1) of the lease:");
            for (int i = 1; i <= pplus1; i++)
                dummy += (year + i) + "\t";
            list.Add(dummy);
            for (int i = 0; i < pricegrid; i++)
            {
                dummy = "";
                for (int j = 0; j < pplus1; j++)
                    dummy += prices[i, j].ToString("N2") + "\t";
                list.Add(dummy);
            }
            list.Add("\nPossible real prices during primary term (+1) of the lease");
            dummy = "";
            for (int i = 1; i <= pplus1; i++)
                dummy += (year + i) + "\t";
            list.Add(dummy);
            for (int i = 0; i < pricegrid; i++)
            {
                dummy = "";
                for (int j = 0; j < pplus1; j++)
                    dummy += pricesreal[i, j].ToString("N2") + "\t";
                list.Add(dummy);
            }
            list.Add("\nYearly real trendprice starting in " + (year + 1));
            dummy = "";
            foreach (double price in trendprice)
                dummy += price.ToString("N2") + " ";
            list.Add(dummy);
            list.Add("\nPossible field sizes and probabilities:\n");
            list.Add("Fieldsize\tProbabilities");
            for (int i = 0; i < reservegrid + 1; i++)
                list.Add(fielddata[i, 0].ToString("N2") + "\t\t" + fielddata[i, 1].ToString("N3"));
            list.Add("\n*** SOLUTION ***\nE = Explore, W = Wait, D = Develop, A = Abandon, N = No Action\n");
            list.Add("Optimal decision at start is: " + explDecision.action);
            list.Add("Private Value: " + explDecision.privvalue.ToString("N2") +
                " Social Value: " + explDecision.socialval.ToString("N2") + "\n");
            // added for Sarah 10/17/2012
            list.Add(aDecision.action + ": Private Value: " + aDecision.privvalue.ToString("N2")
                + " Social Value: " + aDecision.socialval.ToString("N2"));
            list.Add(wDecision.action + ": Private Value: " + wDecision.privvalue.ToString("N2")
                + " Social Value: " + wDecision.socialval.ToString("N2"));
            list.Add(eDecision.action + ": Private Value: " + eDecision.privvalue.ToString("N2")
                + " Social Value: " + eDecision.socialval.ToString("N2"));
            //
            list.Add("Solution for an unexplored tract");
            dummy = "\t";
            for (int i = 1; i <= explterm; i++)
                dummy = dummy + (year + i) + "\t";
            list.Add(dummy);
            for (int i = 0; i < pricegrid; i++)
            {
                dummy = "\n";
                for (int j = 0; j < explterm; j++)
                    dummy = dummy + "\t" + solution[i, reservegrid + 1, j].action;
                list.Add(dummy);
            }
            list.Add("\n\nMarginal (top) and cumulative (bottom) probability of exploration");
            dummy = "";
            for (int i = 1; i <= explterm; i++)
                dummy = dummy + (year + i) + "\t";
            list.Add(dummy);
            for (int i = 0; i < 2; i++)
            {
                dummy = "\n";
                for (int j = 0; j < explterm; j++)
                    dummy = dummy + probexplore[j, i].ToString("N2") + "\t";
                list.Add(dummy);
            }
            list.Add("\n\nPrivate value of these decisions, in current dollars, are:");
            for (int i = 0; i < pricegrid; i++)
            {
                dummy = "\n";
                for (int j = 0; j < explterm; j++)
                    dummy = dummy + solution[i, reservegrid + 1, j].privvalue.ToString("e3") + "\t";
                list.Add(dummy);
            }

            list.Insert(0, "Date: " + currentRunDateTime);

        }
        private void printFullSolution(ArrayList list)
        {
            string dummy;
            list.Add("\"**************FULL SOLUTION**************\"");
            for (int i = 0; i < pplus1; i++)
                for (int j = 0; j < pricegrid; j++)
                    for (int k = 1; k <= reservegrid + 4; k++)
                    {
                        dummy = "Y = " + (year + i + 1).ToString() + " Price = " + prices[j, i].ToString("N2");
                        if (k <= reservegrid + 1) // convert to string
                            dummy += " Resr.= " + fielddata[k - 1, 0].ToString("e3") + " ";
                        else if (k == reservegrid + 2)
                            dummy += " Unexplored ";
                        else if (k == reservegrid + 3)
                            dummy += " Abandoned ";
                        else
                            dummy += " Developed ";
                        dummy += " PV = " + solution[j, k - 1, i].privvalue.ToString("e3") + " SV = " +
                            solution[j, k - 1, i].socialval.ToString("e3") + " Action = " + solution[j, k - 1, i].action;
                        list.Add(dummy);
                    }
            checkprices(list);
            list.Add("\n\nPrice transition probabilities\"");
            printprobs(list);

        }
        private void printprobs(ArrayList list)
        {
            for (int i = 0; i < pplus1; i++)
            {
                string temp = "";
                list.Add("\n\nYear: " + (i + 1));
                for (int j = 0; j < pricegrid; j++)
                {
                    temp = "\n";
                    for (int k = 0; k < pricegrid; k++)
                        temp = temp + priceprob[j, k, i].ToString("N1") + " ";
                    list.Add(temp);
                }
            }
        }
        private void checkprices(ArrayList list)
        {
            for (int i = 0; i < pricegrid; i++)
                for (int j = 0; j < pplus1; j++)
                {
                    string ofprices = " ";
                    list.Add("\ngrid " + i + " period " + j);
                    for (int k = 0; k < produceyears; k++)
                        ofprices = ofprices + Math.Round(exprices[i, j, k], 2) + " ";
                    list.Add(ofprices);
                }
        }
        private void printdelay(ArrayList list)
        {
            list.Add("\n--- Delay Results ---\n\nPresent value of lease(" + year +
                " dollars) from private and social perspectives as a function of years delay in lease issuance:\n\n" +
                "\tYears Delay\t$Private\t$Social");
            for (int i = 0; i < delayyears / delayincr + 1; i++)
                list.Add("\t" + delayresults[i, 0].ToString() + "\t" + delayresults[i, 1].ToString("E5") +
                    "\t" + delayresults[i, 2].ToString("E5"));
        }
        public void batchfunction()
        {
            string temp;
            string[][] resultInputs;
            char[] delimiterChars = { ' ' };
            decision batchoptim;
            ArrayList batchlist = new ArrayList();
            int tempperiods = periods, tempexplterm = explterm, tempfirstyrexpl = yrfirstexpl, tempyrtodev = yrtodev, temppplus1 = pplus1;
            readBatchInputs(batchfile);
            resultInputs = convertInputs(delimiterChars, batchArray);
            batchlist.Add("WEB2 Batch Mode\n" + "fill later Inputs File Name\n" + batchfile + " Batch File Name\n" +
                "Prob\tMeansize\tSD\tATNPV\tNEV");
            for (int i = 0; i < batchArray.Length; i++)
            {
                temp = "";
                probsucc = double.Parse(resultInputs[i][0]);
                fieldsize = double.Parse(resultInputs[i][1]);
                fieldsd = double.Parse(resultInputs[i][2]);
                if (probsucc > 0)
                {
                    makefield();
                    optimizeback(solution);
                    batchoptim = optimizestart(solution);
                    if (delayyears != 0)
                    {
                        delayanalysis();
                        periods = tempperiods;
                        explterm = tempexplterm;
                        yrfirstexpl = tempfirstyrexpl;
                        yrtodev = tempyrtodev;
                        pplus1 = temppplus1;
                    }
                    temp += probsucc.ToString("N5") + "\t" + fieldsize.ToString("E5") + "\t" + fieldsd.ToString("E5") +
                        "\t" + batchoptim.privvalue.ToString("E5") + "\t" + batchoptim.socialval.ToString("E5") + "\t";
                    if (delayyears > 0)
                        for (int bcc = 2; bcc < delayyears / delayincr + 1; bcc++)
                            temp += delayresults[bcc - 1, 0].ToString("N2") + "\t" + delayresults[bcc - 1, 1].ToString("E5") +
                                "\t" + delayresults[bcc - 1, 2].ToString("E5");
                    batchlist.Add(temp);
                }
                else // prospect has no resources so skip and fill in the blanks
                {
                    batchlist.Add(probsucc.ToString("N5") + "\t" + fieldsize.ToString("E5") + "\t" + fieldsd.ToString("E5") +
                        "\t0\t0");
                    if (delayyears > 0)
                        for (int bcc = 2; bcc < delayyears / delayincr + 1; bcc += delayincr)
                            batchlist.Add("0.00\t0\t0");
                }
            }
            string[] printArray = batchlist.ToArray(typeof(string)) as string[];
            writeOutput(printArray, currDirectory + "batchresults.txt", 0, 1);
        }
        public void normalfunction(int hurdleCheck, int printFinal)
        {
            makefield();
            optimizeback(solution);
            explDecision = optimizestart(solution);
            if (explterm >= 1)  // only compute exploration prob if >= 1 year
                probexploration();
            printsolution(hurdleCheck, printFinal);   // print solution
        }
        public void oecm(double price, int compYear)
        {   // 06/22/2017 oecm subtraction sarah
            try
            {
            List<double> socialModOECM = new List<double>();
            List<double> priceList = new List<double>();
            List<int> maxYearOECM = new List<int>();
            double oecmHurdle;
            resultsViewer53.Add("OECM Adds:");
            for (int i = 0; i < delayresults.GetLength(0); i++) 
            {                   
                if (OECMSocialMod.Count < delayresults.GetLength(0))
                    OECMSocialMod.Add(0);
                resultsViewer53.Add(i.ToString() + " :: " + OECMSocialMod[i].ToString());
            }
            
            for (int i = 0; i < socialCostsIncremented.Count; i++)
            {  
                priceList.Add(price);
                price++;
                for (int j = 0; j < socialCostsIncremented[i].Count; j++)
                {
                    socialCostsSubtractOECM[i][j] = socialCostsIncremented[i][j] - OECMSocialMod[j];
                }
                maxYearOECM.Add(optimalHurdleYearOECM(socialCostsSubtractOECM[i]));
                resultsViewer53.Add("OECM Increment Price: " + priceList[i] + " OECM Max Year: " + maxYearOECM[i]);
            }
            oecmHurdle = optimalHurdleSolutionOECM(maxYearOECM, priceList, compYear);
            resultsViewer53.Add("OECM Hurdle Price Solution: " + oecmHurdle);

            }
            catch 
            {
                MessageBox.Show("OECM Input Error");
            }
        }


        public void hurdlefunction(int useweb30)
        {

                // 6/22/2017 update OECM Social Cost Subtraction & recalc
                double incrementPrice = userStartPrice;
                double[,] delayMatrixResults = new double[userNumberLoops, 2];
                resultsViewer53.Add("Social Cost\tYear");
                for (int i = 0; i < userNumberLoops; i++)           // running WEB3 multiple (userNumberLoops) times, at user (incrementPrice) increments.
                {
                    web30startup(0, useweb30);
                    firstyearprice(incrementPrice);
                    for (int j = 2; j <= pplusdelay; j++)
                        nextyrpr(j);
                    expectedprice();
                    makereal();
                    normalfunction(0, 0);
                    delayMatrixResults[i, 0] = incrementPrice;
                    delayMatrixResults[i, 1] = optimalHurdleYear(delayresults, 2);
                    incrementPrice += userIncrement;
                    resultsViewer53.Add("Increment Price: " + delayMatrixResults[i, 0] + "  Max Year: " + delayMatrixResults[i, 1]);
                }
                oecm(incrementPrice - userNumberLoops, compareYear);
                hurdleprice = optimalHurdleSolution(delayMatrixResults, 1, 0, compareYear);
                web30startup(1, useweb30);
                web30midsection(hurdleprice);
                normalfunction(1, viewer);
                batchmode = 3;          // note: this breaks console only version hurdle analysis
                resultsViewer53.Add("Output Form Directory: " + currDirectory + "\\" + resultsfile);

        }

        public double optimalHurdleSolution(double[,] searchArray, int searchLoc, int otherLoc, int compareValue)
        {                       // calculates optimal year given user desired direction and compared value  other switches turned off until needed.
            double answerMin = 999;
            for (int i = 0; i <= searchArray.GetLength(0) - 1; i++)
                    if (searchArray[i, searchLoc] < compareValue)           // value is less than compareValue (aka year 5)
                        if (searchArray[i, otherLoc] < answerMin)           // lowest price answer that passes above statement
                            answerMin = searchArray[i, otherLoc];
            return (answerMin);                                             // returns lowest year
        }

        public double optimalHurdleSolutionOECM(List<int> maxYears, List<double> prices, int compareValue)
        {
            double answerMin = 999;
            for (int i = 0; i <= maxYears.Count - 1; i++)
                if (maxYears[i] < compareValue)
                    if (prices[i] < answerMin)
                        answerMin = prices[i];
            return answerMin;
        }

        public int optimalHurdleYear(double[,] searchArray, int searchLoc)
        {        
            // 6/22/2017 update OECM Social Cost Subtraction & recalc
            List<double> socialCostsYearly = new List<double>();

            int yearMax = 0;
            double valueMax = 0;
            for (int i = 0; i <= searchArray.GetLength(0) - 1; i++)
            {
                if (searchArray[i, searchLoc] > valueMax)
                {
                    valueMax = searchArray[i, searchLoc];
                    yearMax = i;                                            // years start at 0, check if i or i+1
                }
                resultsViewer53.Add(searchArray[i, searchLoc] + " " + i);
                socialCostsYearly.Add(searchArray[i, searchLoc]);
            }
            socialCostsSubtractOECM.Add(socialCostsYearly);
            socialCostsIncremented.Add(socialCostsYearly);
            return (yearMax);                                               // returns location of optimal year of array as answer
        }

        public int optimalHurdleYearOECM(List<double> socialCostsList)
        {
            int yearMax = 0;
            double valueMax = 0;

            for (int i = 0; i <= socialCostsList.Count - 1; i++)
            {
                if (socialCostsList[i] > valueMax)
                {
                    valueMax = socialCostsList[i];
                    yearMax = i;
                }
                resultsViewer53.Add(socialCostsList[i].ToString() + " " + i);
            }

            return yearMax;
        }

        public void web30startup(int solveHurdle, int usewebformInputs)
        {
            if (usewebformInputs == 0)
                assignInputs();
            if (solveHurdle == 1)
                batchmode = 1;

            computeInputs(1);
            declareArray();
            calctrendprice();
            makemult(errors, pricegrid, sigma);
        }

        public void web30midsection(double startingPrice)
        {
            firstyearprice(startingPrice);
            for (int i = 2; i <= pplusdelay; i++)
                nextyrpr(i);
            expectedprice();
            makereal();
        }

        private void writeHurdleResults(ArrayList list)
        {
            resultsViewer53.Add("Solution Price Calculated at: $" + hurdleprice.ToString("N2"));
            list.Add("********** Hurdle Analysis Results **********");
            list.Add("Hurdle Results were calculated using the Solution Price of: $" + hurdleprice.ToString("N2") + ", not the starting price of oil listed in the inputs.");
            if (hurdleprice == 999)
                list.Add("Hurdle Results did not find a solution ($999), select a different set of Initial Prices & Iterations.");
            if (hurdleprice == userStartPrice)
                list.Add("Hurdle Solution Price is the same as user-inputted Initial Price, check results with a lower Initial Price to verify results.");
            list.Add("Output Form Directory: " + currDirectory + "\\" + resultsfile);
        }

        public void runweb30console(Input myObject, string inputfileName)
        {
            char[] delimiter = { '\"' };
            myObject.readInputs(inputfileName);
            myObject.sortInputs = myObject.convertInputs(delimiter, myObject.inputArray);
            myObject.web30startup(0, 0);
            if (myObject.batchmode == 3)    // Hurdle Analysis 2016, reusing existing code instead of rewriting
                myObject.hurdlefunction(0);
            else
            {
                myObject.web30midsection(myObject.startprice);
                if (myObject.batchmode == 1)  // Normal Analysis
                    myObject.normalfunction(0, 0);
                else if (myObject.batchmode == 2) // Batch Analysis
                    myObject.batchfunction();
                else
                    Console.WriteLine("fix batchmode 1 or 2 in input file");
            }

            Console.WriteLine("Output Directory: " + myObject.currDirectory + "\\" + myObject.resultsfile);
        }

        public void runweb30form()
        {            
            web30startup(0, 1);
            hurdlefunction(1); // Hurdle Analysis 2016, reusing existing code instead of rewriting

        }

    } // end of class
}
