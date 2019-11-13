       IDENTIFICATION DIVISION.
       program-id. Pg4_FHood.
       author. Forrest Hood.

      * * * * * *
      * This program sorts (by warehouse, vendor, and product id) three
      * separate inventory files and merges them into one file. It then 
      * uses the merged file to give a detailed listing of all the oil 
      * and cream inventory in the three warehouses.
      * Keys for sorting/merging/breaking are: Warehouse - major, 
      * vendor - intermediate, and product - minor.
      * Determines the total cost spent for each product, vendor, 
      * warehouse, and a grand total.
      * * * * * *

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

            SELECT Unsorted-File1
                Assign to "PR4F19-NV10.txt"
            Organization is line sequential.

            SELECT Unsorted-File2
                Assign to "PR4F19-CA20.txt"
            Organization is line sequential.

            SELECT Unsorted-File3
                Assign to "PR4F19-WA30.txt"
            Organization is line sequential.

            SELECT Sorted-File1
                Assign to "NV10-Sorted.txt"
            Organization is line sequential.

            SELECT Sorted-File2
                Assign to "CA20-Sorted.txt"
            Organization is line sequential.

            SELECT Sorted-File3
                Assign to "WA30-Sorted.txt"
            Organization is line sequential.

            SELECT Merged-File
                Assign to "Merged-Warehouses.txt"
            Organization is line sequential.

            SELECT Inventory-Report
                Assign to printer "Warehouse-Report".

            SELECT Error-File
                Assign to printer "Inventory-Errors".

            SELECT SortMerge-File
                Assign to "SortMerge.tmp".

       DATA DIVISION.
       FILE SECTION.
             