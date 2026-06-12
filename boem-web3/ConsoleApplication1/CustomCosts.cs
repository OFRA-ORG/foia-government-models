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
    public partial class CustomCosts : Form
    {
        public string[] userCost = new string[16];
        public string[] labels = new string[16];
        //string defCosts = "Example Name\n1\n0\n515030000\n1\n4.125\n0\n0\n1.7\n15170220\n2\n1\n14180000\n0\n1\n1";

        public CustomCosts()
        {
            InitializeComponent();
        }
        public CustomCosts(string[] passedLabels)
        {
            InitializeComponent();
            labels = passedLabels;
        }

        private void buttonSaveUserCosts_Click(object sender, EventArgs e)
        {
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

            if (userCost == null)
                userCost[0] = "empty";
            this.Close();

        }
        
        private void CustomCosts_Load(object sender, EventArgs e)
        {
            textCostsName.Text = labels[15] + " Copy";
            textCostsBK.Text = labels[0];
            textCostsCK.Text = labels[1];
            textCostsDK.Text = labels[2];
            textCostsFT.Text = labels[3];
            textCostsOP.Text = labels[4];
            textCostsAlpha.Text = labels[5];
            textCostsBeta.Text = labels[6];
            textCostsTran.Text = labels[7];
            textCostsFFAC1.Text = labels[8];
            textCostsFFAC2.Text = labels[9];
            textCostsREFFIELD.Text = labels[10];
            textCostsEXPLCOST.Text = labels[11];
            textCostsPRODLAG.Text = labels[12];
            textCostsRSP1.Text = labels[13];
            textCostsRSP2.Text = labels[14];
        }

        private void CustomCosts_FormClosing(object sender, FormClosingEventArgs e)
        {
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
            if (userCost == null)
                userCost[0] = "empty";
        
        }



   
    }
}
