namespace WEB30
{
    partial class ResultsViewer
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.richTextBoxResultsViewer = new System.Windows.Forms.RichTextBox();
            this.SuspendLayout();
            // 
            // richTextBoxResultsViewer
            // 
            this.richTextBoxResultsViewer.Dock = System.Windows.Forms.DockStyle.Fill;
            this.richTextBoxResultsViewer.Location = new System.Drawing.Point(0, 0);
            this.richTextBoxResultsViewer.Margin = new System.Windows.Forms.Padding(2, 3, 2, 3);
            this.richTextBoxResultsViewer.Name = "richTextBoxResultsViewer";
            this.richTextBoxResultsViewer.Size = new System.Drawing.Size(920, 562);
            this.richTextBoxResultsViewer.TabIndex = 0;
            this.richTextBoxResultsViewer.Text = "";
            // 
            // ResultsViewer
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(7F, 16F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(920, 562);
            this.Controls.Add(this.richTextBoxResultsViewer);
            this.Font = new System.Drawing.Font("Arial", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.Margin = new System.Windows.Forms.Padding(2, 3, 2, 3);
            this.Name = "ResultsViewer";
            this.Text = "Results Viewer";
            this.Load += new System.EventHandler(this.ResultsViewer_Load);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.RichTextBox richTextBoxResultsViewer;
    }
}