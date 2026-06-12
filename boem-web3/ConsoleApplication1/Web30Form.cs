using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;


namespace WEB30
{


    public partial class Web30Form : Form
    {      
        double[][] costMatrix = new double[10][];
        double[] acrsCustom = new double[8];
        List<Input> allWEB30Inputs = new List<Input>();
        List<double[]> costList = new List<double[]>();
        Dictionary<int, string> costDictionary = new Dictionary<int, string>();
        int numregion;
        

        public Web30Form()
        {
            InitializeComponent();
            assignCostName();
            loadRegionComboBox();
            loadhardcodedCosts();
            greyHurdleCheckbox();
        }
        
        public void assignInputs(Input userObject)
        {
            userObject.title = textBoxTitle.Text;
            userObject.resultsfile = textBoxResultsTitle.Text;
            userObject.region = comboBoxRegionSelect.Text;
            DateTime localdt = DateTime.Now;
            userObject.currentRunDateTime = localdt.ToString();
            if (checkBoxResultsViewer.Checked == true)
                userObject.viewer = 1;

            userObject.year = int.Parse(textBoxCurrYear.Text);
            userObject.pricegrid = int.Parse(textBoxPriceLevel.Text);
            userObject.reservegrid = int.Parse(textBoxReserveGrids.Text);
            userObject.delayyears = int.Parse(textBoxDelayAnalysis.Text);
            userObject.delayincr = int.Parse(textBoxDelayIncrement.Text);
            if (radioButtonPrintHalf.Checked == true)
                userObject.printsize = 1;
            else
                userObject.printsize = 2;
            if (checkBoxHurdleAnalysis.Checked == true)
            {
                userObject.batchmode = 3;
                userObject.userStartPrice = double.Parse(textBoxHurdlePriceInitial.Text);
                userObject.userIncrement = double.Parse(textBoxHurdlePriceIncrement.Text);
                userObject.userNumberLoops = int.Parse(textBoxHurdlePriceIterations.Text);
                userObject.compareYear = int.Parse(textBoxHurdleCompareYear.Text);
            }
            else
                userObject.batchmode = 1;
            userObject.batchfile = "obsolete.txt";
            userObject.pricefile = "web2.pri";

            userObject.periods = int.Parse(textBoxLeasePrimaryTerm.Text);
            userObject.explterm = int.Parse(textBoxLeaseDiligenceTerm.Text);
            userObject.royalty = double.Parse(textBoxLeaseRoyaltyRate.Text);
            userObject.leasecost = double.Parse(textBoxLeaseAcquisitionCost.Text);
            userObject.rental = double.Parse(textBoxLeaseYearlyRentalRate.Text);
            userObject.yrfirstexpl = int.Parse(textBoxLeaseFirstYearExploration.Text);
            userObject.yrtodev = int.Parse(textBoxLeaseFirstYearDevelopment.Text);

            userObject.privdis = double.Parse(textBoxFinancialRealPrivateDiscountRate.Text);
            userObject.socdis = double.Parse(textBoxFinancialRealSocialDiscountRate.Text);
            userObject.distime = int.Parse(textBoxFinancialDiscountTiming.Text);
            userObject.inflation = double.Parse(textBoxFinancialInflationRate.Text);
            userObject.startprice = double.Parse(textBoxFinancialStartingOilPrice.Text);
            userObject.trendstart = double.Parse(textBoxFinancialStartingTrendPrice.Text);
            userObject.sigma = double.Parse(textBoxFinancialStdDevPriceGrowth.Text);
            userObject.realtrend = double.Parse(textBoxFinancialRealTrendGrowth.Text);
            userObject.pulltotrend = double.Parse(textBoxFinancialPullToTrend.Text);
            userObject.drift = double.Parse(textBoxFinancialDriftFactor.Text);
            userObject.tangportion = double.Parse(textBoxFinancialTangiblePortion.Text);
            userObject.tax = double.Parse(textBoxFinancialTaxRate.Text);

            userObject.acrs[0] = double.Parse(textBoxACRSyear1.Text);
            userObject.acrs[1] = double.Parse(textBoxACRSyear2.Text);
            userObject.acrs[2] = double.Parse(textBoxACRSyear3.Text);
            userObject.acrs[3] = double.Parse(textBoxACRSyear4.Text);
            userObject.acrs[4] = double.Parse(textBoxACRSyear5.Text);
            userObject.acrs[5] = double.Parse(textBoxACRSyear6.Text);
            userObject.acrs[6] = double.Parse(textBoxACRSyear7.Text);
            userObject.acrs[7] = double.Parse(textBoxACRSyear8.Text);

            userObject.fieldsd = double.Parse(textBoxGeoStdDevFieldSize.Text);
            userObject.fieldsize = double.Parse(textBoxGeoExpectedFieldSize.Text);
            userObject.probsucc = double.Parse(textBoxGeoProbabilityOil.Text);
            userObject.declinerate = double.Parse(textBoxGeoExponentialDeclineRate.Text);
            userObject.maxyrsdecline = int.Parse(textBoxGeoMaxYearsDecline.Text);
            userObject.yearstopeak = int.Parse(textBoxGeoYearstoPeak.Text);
            userObject.minatpeak = int.Parse(textBoxGeoMinYearstoPeak.Text);
            userObject.maxatpeak = int.Parse(textBoxGeoMaxYearstoPeak.Text);

            userObject.propdevcost[0] = double.Parse(textBoxPropDevCostsYear1.Text);
            userObject.propdevcost[1] = double.Parse(textBoxPropDevCostsYear2.Text);
            userObject.propdevcost[2] = double.Parse(textBoxPropDevCostsYear3.Text);
            userObject.propdevcost[3] = double.Parse(textBoxPropDevCostsYear4.Text);
            userObject.propdevcost[4] = double.Parse(textBoxPropDevCostsYear5.Text);

            userObject.bk = costList[numregion][0];
            userObject.ck = costList[numregion][1];
            userObject.dk = costList[numregion][2];
            userObject.ftract = costList[numregion][3];
            userObject.opcost = costList[numregion][4];
            userObject.alpha = costList[numregion][5];
            userObject.beta = costList[numregion][6];
            userObject.trancost = costList[numregion][7];
            userObject.ffac1 = costList[numregion][8];
            userObject.ffac2 = costList[numregion][9];
            userObject.reffield = costList[numregion][10];
            userObject.explcost = costList[numregion][11];
            userObject.prodlag = (int)costList[numregion][12];
            userObject.rsp1 = (int)costList[numregion][13];
            userObject.rsp2 = (int)costList[numregion][14];

            allWEB30Inputs.Add(userObject);



        }
        private void loadinputs(Input loadObject)
        {
            textBoxTitle.Text = loadObject.title;
            textBoxResultsTitle.Text = loadObject.resultsfile;
            
            //loadObject.region = comboBox2.Text;
            if (loadObject.viewer == 1)
                checkBoxResultsViewer.Checked = true;
            else
                checkBoxResultsViewer.Checked = false;
            textBoxCurrYear.Text = loadObject.year.ToString();
            textBoxPriceLevel.Text = loadObject.pricegrid.ToString();
            textBoxReserveGrids.Text = loadObject.reservegrid.ToString();
            textBoxDelayAnalysis.Text = loadObject.delayyears.ToString();
            textBoxDelayIncrement.Text = loadObject.delayincr.ToString();
            if (loadObject.printsize == 1)
                radioButtonPrintHalf.Checked = true;
            else
                radioButtonPrintFull.Checked = true;

            if (loadObject.batchmode == 3)
            {
                checkBoxHurdleAnalysis.Checked = true;
                textBoxHurdlePriceInitial.Text = loadObject.userStartPrice.ToString();
                textBoxHurdlePriceIncrement.Text = loadObject.userIncrement.ToString();
                textBoxHurdlePriceIterations.Text = loadObject.userNumberLoops.ToString();
                textBoxHurdleCompareYear.Text = loadObject.compareYear.ToString();
            }
            else
                checkBoxHurdleAnalysis.Checked = false;
            
            textBoxLeasePrimaryTerm.Text = loadObject.periods.ToString();
            textBoxLeaseDiligenceTerm.Text = loadObject.explterm.ToString();
            textBoxLeaseRoyaltyRate.Text = loadObject.royalty.ToString();
            textBoxLeaseAcquisitionCost.Text = loadObject.leasecost.ToString();
            textBoxLeaseYearlyRentalRate.Text = loadObject.rental.ToString();
            textBoxLeaseFirstYearExploration.Text = loadObject.yrfirstexpl.ToString();
            textBoxLeaseFirstYearDevelopment.Text = loadObject.yrtodev.ToString();


            textBoxFinancialRealPrivateDiscountRate.Text = loadObject.privdis.ToString();
            textBoxFinancialRealSocialDiscountRate.Text = loadObject.socdis.ToString();
            textBoxFinancialDiscountTiming.Text = loadObject.distime.ToString();
            textBoxFinancialInflationRate.Text = loadObject.inflation.ToString();
            textBoxFinancialStartingOilPrice.Text = loadObject.startprice.ToString();
            textBoxFinancialStartingTrendPrice.Text = loadObject.trendstart.ToString();
            textBoxFinancialStdDevPriceGrowth.Text = loadObject.sigma.ToString();
            textBoxFinancialRealTrendGrowth.Text = loadObject.realtrend.ToString();
            textBoxFinancialPullToTrend.Text = loadObject.pulltotrend.ToString();
            textBoxFinancialDriftFactor.Text = loadObject.drift.ToString();
            textBoxFinancialTangiblePortion.Text = loadObject.tangportion.ToString();
            textBoxFinancialTaxRate.Text = loadObject.tax.ToString();

            textBoxACRSyear1.Text = loadObject.acrs[0].ToString();
            textBoxACRSyear2.Text = loadObject.acrs[1].ToString();
            textBoxACRSyear3.Text = loadObject.acrs[2].ToString();
            textBoxACRSyear4.Text = loadObject.acrs[3].ToString();
            textBoxACRSyear5.Text = loadObject.acrs[4].ToString();
            textBoxACRSyear6.Text = loadObject.acrs[5].ToString();
            textBoxACRSyear7.Text = loadObject.acrs[6].ToString();
            textBoxACRSyear8.Text = loadObject.acrs[7].ToString();

            textBoxGeoStdDevFieldSize.Text = loadObject.fieldsd.ToString();
            textBoxGeoExpectedFieldSize.Text = loadObject.fieldsize.ToString();
            textBoxGeoProbabilityOil.Text = loadObject.probsucc.ToString();
            textBoxGeoExponentialDeclineRate.Text = loadObject.declinerate.ToString();
            textBoxGeoMaxYearsDecline.Text = loadObject.maxyrsdecline.ToString();
            textBoxGeoYearstoPeak.Text = loadObject.yearstopeak.ToString();
            textBoxGeoMinYearstoPeak.Text = loadObject.minatpeak.ToString();
            textBoxGeoMaxYearstoPeak.Text = loadObject.maxatpeak.ToString();


            textBoxPropDevCostsYear1.Text = loadObject.propdevcost[0].ToString();
            textBoxPropDevCostsYear2.Text = loadObject.propdevcost[1].ToString();
            textBoxPropDevCostsYear3.Text = loadObject.propdevcost[2].ToString();
            textBoxPropDevCostsYear4.Text = loadObject.propdevcost[3].ToString();
            textBoxPropDevCostsYear5.Text = loadObject.propdevcost[4].ToString();
            
            costList[9][0] = loadObject.bk;
            costList[9][1] = loadObject.ck;
            costList[9][2] = loadObject.dk;
            costList[9][3] = loadObject.ftract;
            costList[9][4] = loadObject.opcost;
            costList[9][5] = loadObject.alpha;
            costList[9][6] = loadObject.beta ;
            costList[9][7] = loadObject.trancost;
            costList[9][8] = loadObject.ffac1;
            costList[9][9] = loadObject.ffac2;
            costList[9][10] = loadObject.reffield;
            costList[9][11] = loadObject.explcost;
            costList[9][12] = loadObject.prodlag;
            costList[9][13] = loadObject.rsp1;
            costList[9][14] = loadObject.rsp2;

            //allWEB30Inputs.Add(loadObject);   don't forget to load costList on file loading
        }

        private void assignCostName()
        {
            costDictionary.Add(0, "ALARC");
            costDictionary.Add(1, "ALD");
            costDictionary.Add(2, "ALS");
            costDictionary.Add(3, "ATLD");
            costDictionary.Add(4, "ATLS");
            costDictionary.Add(5, "GOMD");
            costDictionary.Add(6, "GOMS");
            costDictionary.Add(7, "PACD");
            costDictionary.Add(8, "PACS");
            costDictionary.Add(9, "Custom");
        }

        private void loadRegionComboBox()
        {                                   // hard coded cost files
            for (int i = 0; i <= 9; i++)
                comboBoxRegionSelect.Items.Add(costDictionary[i]);
        }

        private void loadhardcodedCosts()
        {                                   // BK CK DK FT OP$ ALPHA BETA TRAN$ FFAC1 FFAC2 REFFIELD EXPLCOST PRODLAG RSP1 RSP2
             costMatrix[0] = new double[15] { 0, 0, 2344700000, 1, 2.9, 0, 0, 8.22, 0, 0, 1, 89200000, 0, 1, 1};
             costMatrix[1] = new double[15] { 0.5, 0, 250000000, 1, 3.9, 105000, 0.85, 8.1, 0, 2, 150000000, 4000000, 0, 1, 1};
             costMatrix[2] = new double[15] { 0.5, 0, 100000000, 1, 5.7, 65000, 0.85, 3.9, 0, 2, 50000000, 4000000, 0, 1, 1};
             costMatrix[3] = new double[15] { 0.5, 0, 200000000, 1, 0.6, 100000, 0.85, 0.7, 0, 2, 105000000, 4000000, 0, 1, 1};
             costMatrix[4] = new double[15] { 0.5, 0, 40000000, 1, 0.6, 50000, 0.85, 0.7, 0, 2, 10000000, 4000000, 0, 1, 1};
             costMatrix[5] = new double[15] { 0, 0, 2131460000, 1, 6.08, 0, 0, 2.39, 0, 0, 1, 105320000, 0, 1, 1 };
             costMatrix[6] = new double[15] { 0.5, 0, 20000000, 1, 2, 40000, 0.85, 0.5, 0, 2, 7000000, 4000000, 0, 1, 1};
             costMatrix[7] = new double[15] { 0.5, 0, 250000000, 1, 2, 105000, 0.85, 0.45, 0, 2, 61000000, 4000000, 0, 1, 1};
             costMatrix[8] = new double[15] { 0.5, 0, 20000000, 1, 2.7, 45000, 0.85, 0.45, 0, 2, 7000000, 4000000, 0, 1, 1};
             costMatrix[9] = new double[15] { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1};
            // these are not correct costs
             //costMatrix[5] = new double[15] { 0, 0, 515030000, 1, 4.125, 0, 0, 1.7, 15170220, 2, 1, 14180000, 0, 1, 1 };
                                            // eventually will save/load costs to file
             for (int i = 0; i < 10; i++)
                 costList.Add(costMatrix[i]);
             comboBoxRegionSelect.SelectedIndex = 5;
        }
        
        private void comboBox2_SelectedIndexChanged(object sender, EventArgs e)
        {       //ALARC	ALD	ALS	ATLD	ATLS	GOMD	GOMS	PACD	PACS
            labelCostsRegionTitle.Text = comboBoxRegionSelect.Text;
                textCostsName.Text = labelCostsRegionTitle.Text;
            numregion = comboBoxRegionSelect.SelectedIndex;
            labelCostRegion(numregion);
        }

        private void labelCostRegion(int regionNum)
        {       // tried looping through groupbox, label order inconsistent
            labelcostsbk.Text = costList[regionNum][0].ToString();
            labelcostsck.Text = costList[regionNum][1].ToString();
            labelcostsdk.Text = costList[regionNum][2].ToString();
            labelcostsft.Text = costList[regionNum][3].ToString();
            labelcostsop.Text = costList[regionNum][4].ToString();
            labelcostsalpha.Text = costList[regionNum][5].ToString();
            labelcostsbeta.Text = costList[regionNum][6].ToString();
            labelcoststran.Text = costList[regionNum][7].ToString();
            labelcostsffac1.Text = costList[regionNum][8].ToString();
            labelcostsffac2.Text = costList[regionNum][9].ToString();
            labelcostsreffield.Text = costList[regionNum][10].ToString();
            labelcostsexplcost.Text = costList[regionNum][11].ToString();
            labelcostsprodlag.Text = costList[regionNum][12].ToString();
            labelcostsrsp1.Text = costList[regionNum][13].ToString();
            labelcostsrsp2.Text = costList[regionNum][14].ToString();

            textCostsBK.Text = labelcostsbk.Text;
            textCostsCK.Text = labelcostsck.Text;
            textCostsDK.Text = labelcostsdk.Text;
            textCostsFT.Text = labelcostsft.Text;
            textCostsOP.Text = labelcostsop.Text;
            textCostsAlpha.Text = labelcostsalpha.Text;
            textCostsBeta.Text = labelcostsbeta.Text;
            textCostsTran.Text = labelcoststran.Text;
            textCostsFFAC1.Text = labelcostsffac1.Text;
            textCostsFFAC2.Text = labelcostsffac2.Text;
            textCostsREFFIELD.Text = labelcostsreffield.Text;
            textCostsEXPLCOST.Text = labelcostsexplcost.Text;
            textCostsPRODLAG.Text = labelcostsprodlag.Text;
            textCostsRSP1.Text = labelcostsrsp1.Text;
            textCostsRSP2.Text = labelcostsrsp2.Text;
        }

        private void checkBoxHurdleAnalysis_CheckedChanged(object sender, EventArgs e)
        {
            if (checkBoxHurdleAnalysis.Checked == true)
            {
                textBoxHurdlePriceIncrement.Enabled = true;
                textBoxHurdlePriceIterations.Enabled = true;
                textBoxHurdlePriceInitial.Enabled = true;
                textBoxHurdleCompareYear.Enabled = true;
                textBoxFinancialStartingOilPrice.Enabled = false;
            }
            else
            {
                textBoxHurdlePriceIncrement.Enabled = false;
                textBoxHurdlePriceIterations.Enabled = false;
                textBoxHurdlePriceInitial.Enabled = false;
                textBoxHurdleCompareYear.Enabled = false;
                textBoxFinancialStartingOilPrice.Enabled = true;
            }
        }

        private void greyHurdleCheckbox()
        {
            string currDirectory = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetEntryAssembly().Location);
            checkBoxHurdleAnalysis.Checked = true;
            radioButtonPrintHalf.Checked = true;
            checkBoxResultsViewer.Checked = true;
            textBoxFinancialStartingOilPrice.Enabled = false;
            labelRunsSavedLocation.Text = "Current Save Directory: " + currDirectory + "\\";
        }

        private void Web30Form_Load(object sender, EventArgs e)
        {

        }

        private void assignOECM(Input userObject)
        {
            string temp = "";
            var lines = OECMMultilineTextBox.Lines;
            foreach (string row in lines)
                if (row != "")
                {
                    temp = row;
                    temp = temp.Replace("$", string.Empty);
                    temp = temp.Replace(",", string.Empty);
                    userObject.OECMSocialMod.Add(double.Parse(temp));
                }
        }

        private void buttonRunWEB30_Click(object sender, EventArgs e)
        {
            try          
            {
                Input formInput = new Input();
                assignInputs(formInput);
                assignOECM(formInput);
                formInput.runweb30form();
                populateListBox();
                if (checkBoxLogicViewer.Checked == true)
                {
                    string viewerTitle = "WEB 3.0 Logic Viewer";
                    ResultsViewer logicOutput = new ResultsViewer(formInput.resultsViewer53.ToArray(), viewerTitle);
                    logicOutput.Width = 350;
                    logicOutput.Show();
                }
            }
            catch 
            {
                MessageBox.Show("An input is in the wrong format");
            }
            labelRunsSavedLocation.Text = "";
        }

        private void buttonGoCustomCosts_Click(object sender, EventArgs e)
        {
            string[] userCost = new string[16];
            userCost[0] = textCostsName.Text;
            userCost[1] = textCostsBK.Text;
            userCost[2] = textCostsCK.Text;
            userCost[3] = textCostsDK.Text;
            userCost[4] = textCostsFT.Text;
            userCost[5] = textCostsOP.Text;
            userCost[6] = textCostsAlpha.Text;
            userCost[7] = textCostsBeta.Text;
            userCost[8] = textCostsTran.Text;
            userCost[9] = textCostsFFAC1.Text;
            userCost[10] = textCostsFFAC2.Text;
            userCost[11] = textCostsREFFIELD.Text;
            userCost[12] = textCostsEXPLCOST.Text;
            userCost[13] = textCostsPRODLAG.Text;
            userCost[14] = textCostsRSP1.Text;
            userCost[15] = textCostsRSP2.Text;

            addCosts(userCost);
        }

        public void addCosts(string[] userCosts)
        {
            if (userCosts[0] == "empty")
            {}
            else
            {
                costDictionary.Add(costDictionary.Count() + 1, userCosts[0]);
                int dictCount = costDictionary.Count() - 1;
                double[] containsCosts = new double[15];
                for (int i = 0; i < containsCosts.Length; i++)
                    containsCosts[i] = (double.Parse(userCosts[i + 1]));
                costList.Add(containsCosts);
                comboBoxRegionSelect.Items.Add(costDictionary[costDictionary.Count()]);
                comboBoxRegionSelect.SelectedIndex = dictCount;
            }
        }

        private void populateListBox()
        {
            List<string> stringlist = new List<string>();
            foreach (Input savedrun in allWEB30Inputs)
                stringlist.Add(savedrun.title + "\t" + savedrun.currentRunDateTime);
            listBoxSavedRuns.DataSource = stringlist;
        }

        private void buttonRunsDeleteInputs_Click(object sender, EventArgs e)
        {
            if (allWEB30Inputs.Count > 0)
            {
                allWEB30Inputs.RemoveAt(listBoxSavedRuns.SelectedIndex);
                populateListBox();

            }
            labelRunsSavedLocation.Text = "";
        }

        private void checkBoxResultsViewer_CheckedChanged(object sender, EventArgs e)
        {

        }

        private void buttonRunsViewResults_Click(object sender, EventArgs e)
        {
            if (allWEB30Inputs.Count > 0)
            {
                string[] passme = allWEB30Inputs[listBoxSavedRuns.SelectedIndex].outputArray53;
                string viewerTitle = "WEB 3.0 Results Viewer";
                ResultsViewer loadForm = new ResultsViewer(passme, viewerTitle);
                loadForm.ShowDialog();
            }
        }

        private void buttonRunsLoadInputs_Click(object sender, EventArgs e)
        {
            if (allWEB30Inputs.Count > 0)
            {
                loadinputs(allWEB30Inputs[listBoxSavedRuns.SelectedIndex]);
                tabControl1.SelectedIndex = 0;
            }
        }

        private void buttonSaveInputs_Click(object sender, EventArgs e)
        {
            string currDirectory = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetEntryAssembly().Location);
            if (allWEB30Inputs.Count > 0)
            {
                SaveFileDialog myInDialog = new SaveFileDialog();
                myInDialog.InitialDirectory = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetEntryAssembly().Location);
                myInDialog.Filter = "WEB3 XML (*.xml)|*.xml|AllFiles(*.*)|*.*";
                myInDialog.CheckFileExists = false;
                string inputTableName;
                if (myInDialog.ShowDialog() == DialogResult.OK)
                {
                    inputTableName = myInDialog.FileName;
                    SaveXML saveME = new SaveXML();
                    saveME.SerializeObject(allWEB30Inputs, inputTableName);
                    labelRunsSavedLocation.Text = "Saved to: " + inputTableName;
                }
            }
             
        }

        private void buttonLoadInputs_Click(object sender, EventArgs e)
        {
            
            OpenFileDialog myInDialog = new OpenFileDialog();
            myInDialog.InitialDirectory = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetEntryAssembly().Location);
            myInDialog.Filter = "WEB3 XML (*.xml)|*.xml|AllFiles(*.*)|*.*";
            myInDialog.CheckFileExists = true;
            myInDialog.Multiselect = false;
            string inputTableName;
            if (myInDialog.ShowDialog() == DialogResult.OK)
            {
                inputTableName = myInDialog.FileName;
                SaveXML loadME = new SaveXML();
                allWEB30Inputs = loadME.DeSerializeObject(inputTableName);
                populateListBox();
                labelRunsSavedLocation.Text = "Loaded from: " + inputTableName;
            }
        }

        private void textBoxHurdlePriceIncrement_TextChanged(object sender, EventArgs e)
        {

        }

        private void tabMainControl_Click(object sender, EventArgs e)
        {

        }

        private void textCostsName_TextChanged(object sender, EventArgs e)
        {

        }

    }



}


