Although the Power Query in Excel and the ability of Word 2013+ to open PDF files gets closer to allowing people in finance roles to automate data retrieval for the invoice 
validation and account reconciliation processes. There is always a lot of PDFs that just don't seem to be readable, returning incomplete results or just demanding too much effort
to make the process almost worthless. Even having Adobe Full version fails to deliver useful extracts in such cases.

The attached PDF is not a true representation of what to expect when extracting tables from the numerous Statements and Invoices in finance, but it gives a good testing ground to 
learn the effectiveness of R Programming libraries in comparison to Excel's Power Query, Word converters and Adobe's export to Excel functions.

When adapting my original macro functions in a new Excel macro driven file I noted that several issues prevent the process of automating extracting tables PDF being fully automatic.
  - Both Power Query and R Scripts require you to remove additional tables when they occur
  - Special Characters that occur in the data may have actually been part of the original content

As text based files do not produce the same Table structure when creating a Data Connection to them the VBA Script I wrote to use an ADODB connection and then populate a table structure takes as long as the Power Query to refresh it's tables.

However, as you will note. 
  - Power Query was not able to identify all linefeeds correctly and some entries were concatenated without the correct spacing 
  - Creating a data connection on the Excel macro file and joining the two tabs also highlights that not all linefeeds are picked up by the MHTML convertion and Power Query

WARN Report Rendering
  - R Script
    - Convert PDF to csv:	5 minutes 5 seconds
    - Total Time to Render:	6 minutes 2 seconds
  - Microsoft Word conversion and Power Query rendering
    - Convert PDF to mhtml:	2 minutes 17 seconds
    - Total Time to Render:	3 minutes (However, time to change Query was immense)
  - TrapRange Java
    - Convert PDF to csv:	52 seconds
    - Total Time to Render:	1 minute 18 seconds

References:

Matt Allington
https://exceleratorbi.com.au/import-tabular-data-pdf-using-power-query/#:~:text=Select%20the%20PDF%20connector%20and%20specify%20the%20path,the%20PDF%20Connector%20in%20Power%20BI%20follows%20below

Troy Walters
https://datascienceplus.com/extracting-tables-from-pdfs-in-r-using-the-tabulizer-package/

Mynda Treacy
https://www.youtube.com/watch?v=Xkew2GrXu9c

For completeness I wanted to compare Python requirements and function but found that Python only uses a Java Wrapper. So I found some Java code called TrapRange and modified it a little to compare Java's requirements, function and end results against the use of R Script and VBA or Power Query and VBA. Admittedly a lot more VBA has been applied to manage the output from TrapRange as I am not as Savvy as the originators of it's code.

I was impressed with the fact that it only took TrapRange 52 seconds to produce the base csv file as opposed to the 5 to 10 minutes Word takes to convert and then save the PDF as an MHTML file and the 5 to 10 minutes Tabulizer running through the R Script also takes. Also, it only took 1 minute 42 seconds to completely render the tables in Excel and update the linked Pivot table. I found that I got better results by getting TrapRange to save the csv as Tab Delimited before opening with VBA to cleanse and save as a CSV file. I can add extra find and replace fields in the Excel worksheet, but would rather that function be included in the Java code.

References:
Tho
https://github.com/thoqbk/traprange
