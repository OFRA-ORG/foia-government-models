using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Collections;
using System.Windows.Forms;

namespace WEB30
{
    class Program
    {
        [STAThread]
        static void Main(string[] args)
        {
            Input myObject = new Input();
            string inputfileName = "";
            foreach (string ice in args)
                inputfileName = ice;
            if (inputfileName == "")
                inputfileName = "C:\\web3\\sarahex.inp";		// defined input file for testing
            //myObject.runweb30console(myObject, inputfileName);
            Application.Run(new Web30Form());
            
        }

    }
}


        /* kevin nguyen updated 07/2012 
         * 
         * notes: 
         *      ACRS schedule updated - no longer limited to 8 years schedule
         *      Proportion of development cost per year should also function like ACRS
         *      Removed reading price inputs (expectedprice) - originally used to save processor speed, not intended for custom vectors
         *          will add custom vectors later
         *      Will use excel/.exe combination to run web2. Inputs in excel, .net coding to launch web2 with excel inputs, send outputs
         *          to text file and excel (hopefully)
         * 
         * */

        /* kevin nguyen update 02/2016
			
           adding additional functions: 
           #1: solving hurdle price	
               objective: run a "batch" type mode at all prices, create array of objects and compare results to find optimal solution
				
         * #2: adding webform to cut out excel input form
         * 
         * #3: modifying social cost formula
				
		 2:57 PM 1/27/2016
         * 
         * #4 moved program.cs (the actual code) into Input.cs
		 
		 12:10 2/4/2016
         * 
         * 2/9/2016
         * completed save/load function to webform
         * 
         * to do: add second results window for logic (console writelines)
         * 
         * 2/10/2016
         * 
         * releasing beta build version
		 
         10:00 5/18/2016
         * Rebuilt Custom Costs, reordered list
         * 
          6/22/2017
         * adding OECM social costs subtraction stuff, custom costs are broken will fix
         * 
         * 7/6/2017
         * 
         * oecm finished and customcosts retooled, cut out customcosts.cs
         * Costs are not being saved into savedruns -- likely to cause future issue
         * 
         * 10/02/2018
         * to do list: 
         * 1) logic viewer automatic saved text results
         * 2) reorganize inputs
         * 3) label costs/smarter units ($ cause errors)
         * 4) costs not saved into savedruns
         * 5) clearer output for hurdle results
         * 6) output file does not print unless resultsviewer selected
         * 7) runs must autosave (crash/error handling protection)
        */

