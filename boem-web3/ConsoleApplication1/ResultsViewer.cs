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
    public partial class ResultsViewer : Form
    {
        public ResultsViewer()
        {
            InitializeComponent();
            richTextBoxResultsViewer.Text = "You forgot something.";
        }
        public ResultsViewer(string[] resultsArray, string titleText)
        {
            InitializeComponent();
            for (int i = 0; i < resultsArray.Length; i++)
                richTextBoxResultsViewer.AppendText(String.Format("{0} {1}", resultsArray[i], Environment.NewLine));
            this.Text = titleText;
        }

        private void ResultsViewer_Load(object sender, EventArgs e)
        {
        }
        

    }

}
