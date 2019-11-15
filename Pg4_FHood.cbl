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

       FD Unsorted-File1
           Record contains 128 characters.

       01  Unsorted-Record1.
           05 UR1-WHID               PIC X(4) .
           05 UR1-Vendor             PIC X .
           05 UR1-Product            PIC X(3) .
           05 Filler                 PIC X(120) .
      *
       FD Unsorted-File2
            Record contains 128 characters.

       01  Unsorted-Record2.
           05 UR2-WHID               PIC X(4) .
           05 UR2-Vendor             PIC X .
           05 UR2-Product            PIC X(3) .
           05 Filler                 PIC X(120) .
      *
       FD Unsorted-File3
            Record contains 128 characters.

       01  Unsorted-Record3.
           05 UR3-WHID               PIC X(4) .
           05 UR3-Vendor             PIC X .
           05 UR3-Product            PIC X(3) .
           05 Filler                 PIC X(120) .
      *
       FD Sorted-File1
           Record contains 128 characters.

       01  Sorted-Record1.
           05 SR1-WHID               PIC X(4) .
           05 SR1-Vendor             PIC X .
           05 SR1-Product            PIC X(3) .
           05 Filler                 PIC X(120) .
      *
       FD Sorted-File2
           Record contains 128 characters.

       01  Sorted-Record2.
           05 SR2-WHID               PIC X(4) .
           05 SR2-Vendor             PIC X .
           05 SR2-Product            PIC X(3) .
           05 Filler                 PIC X(120) .
      *
       FD Sorted-File3
           Record contains 128 characters.

       01  Sorted-Record3.
           05 SR3-WHID               PIC X(4) .
           05 SR3-Vendor             PIC X .
           05 SR3-Product            PIC X(3) .
           05 Filler                 PIC X(120) .
      *
       FD Merged-File
           Record contains 128 characters.

       01  Merged-Record.
           05 MR-WHID                PIC X(4) .
           05 MR-Vendor              PIC X .
           05 MR-Product             PIC X(3) .
           05 MR-Data  OCCURS 5 TIMES.
               10 MRD-Name           PIC X(13) .
               10 MRD-Size           PIC X .
               10 MRD-Type           PIC X .
               10 MRD-Stock          PIC S9(4) .
               10 MRD-Price          PIC S9(3)v99 .
      *
       SD SortMerge-File
           Record contains 128 characters.

       01  SortMerge-Record.
           05 SM-WHID                PIC X(4) .
           05 SM-Vendor              PIC X .
           05 SM-Product             PIC X(3) .
           05 Filler                 PIC X(120) .
      *
       FD Inventory-Report
           Record contains 80 characters.

       01  Report-Line               PIC X(80) .
      *
       FD Error-File
           Record contains 128 characters.

       01  Error-Line                PIC X(128) .

       WORKING-STORAGE SECTION.
       
       01  Flags-n-Switches.
           05 More-Data              PIC X Value 'Y'.
           05 First-Run              PIC X Value 'Y'.

       01  Total-Fields.
           05 Grand-Total            PIC S9(9)v99 Value ZERO.
           05 WH-Total               PIC S9(8)v99 Value ZERO.
           05 Vendor-Total           PIC S9(7)v99 Value ZERO.
           05 Prod-Total             PIC S9(7)v99 Value ZERO.
           05 Error-Total            PIC 99.

       01  Holds.
           05 WH-Hold                PIC X(4).
           05 Vendor-Hold            PIC X.
           05 Product-Hold           PIC X(3).

       01  Misc.
           05 Proper-Spacing         PIC 9.
           05 Sub                    PIC 9.

       01  WS-Current-Date.
           05  WS-Year               PIC 99.
           05  WS-Month              PIC 99.
           05  WS-Day                PIC 99.
      
       01  Vendor-Table-Description.
           05 PIC X(13) Value 'IMadeInHouse'.
           05 PIC X(13) Value 'TTansia Corp.'.
           05 PIC X(13) Value 'AAMEL Ltd'.
           05 PIC X(13) Value 'WWEST Corp'.
           05 PIC X(13) Value 'DDENIO Corp.'.
           05 PIC X(13) Value 'VVISSON Corp.'.
           05 PIC X(13) Value 'NNETON Ltd'.

       01  Vendor-Table REDEFINES Vendor-Table-Description.
           05  Vendor-Item Occurs 7 Times
               Indexed by Vendor-Index.
               10 Vendor-Key         PIC X.
               10 Vendor-Name        PIC X(12).



      * * * Output Area * * *

       01  Heading-One.
           05                    PIC X(35) Value SPACES.
           05                    PIC X(9) Value 'DR. CHEEB'.

       01  Heading-Two.
           05                    PIC X(10) Value SPACES.
           05 H2-Date.
               10  H2-Month      PIC Z9.
               10                PIC X    VALUE '/'.
               10  H2-Day        PIC 99.
               10                PIC X    VALUE '/'.
               10                PIC XX   VALUE'20'.
               10  H2-Year       PIC 99.
           05                    PIC X(12) Value SPACES.
           05                    PIC X(16) Value 'INVENTORY REPORT'.
           05                    PIC X(8) Value SPACES.
           05                    PIC X(6) Value 'PAGE: '.
           05 H2-PageNum         PIC 99 .

       01  Warehouse-Heading.
           05                    PIC XX Value SPACES.
           05                    PIC X(11) Value 'WAREHOUSE: '.
           05 WHID-Heading       PIC X(4).

       01  Vendor-Heading.
           05                    PIC X(5) Value SPACES.
           05                    PIC X(8) Value 'VENDOR: '.
           05 VH-Vendor          PIC X(12).

       01  Detail-Heading-One.
           05                    PIC X(8) Value SPACES.
           05                    PIC X(7) Value 'PRODUCT'.
           05                    PIC X(7) Value SPACES.
           05                    PIC X(8) Value 'PROD    '.
           05                    PIC X(7) Value 'PRODUCT'.
           05                    PIC X(4) Value SPACES.
           05                    PIC X(8) Value 'PROD    '.
           05                    PIC XX   Value 'IN'.
           05                    PIC X(7) Value SPACES.
           05                    PIC X(5) Value 'TOTAL'.

       01  Detail-Heading-Two.
           05                    PIC X(10) Value SPACES.
           05                    PIC X(4) Value 'NAME'.
           05                    PIC X(9) Value SPACES.
           05                    PIC X(2)   Value 'ID'.
           05                    PIC X(6) Value SPACES.
           05                    PIC X(4) Value 'SIZE'.
           05                    PIC X(6) Value SPACES.
           05                    PIC X(7) Value 'TYPE   '.
           05                    PIC X(5) Value 'STOCK'.
           05                    PIC X(5) Value SPACES.
           05                    PIC X(4) Value 'COST'.

       01  Detail-Line.
           05                    PIC X(5) Value SPACES.
           05 DL-Prod-Name       PIC X(13).
           05                    PIC X(4) Value SPACES.
           05 DL-ID              PIC XXX.
           05                    PIC XX  Value SPACES.
           05 DL-Size            PIC X(11).
           05                    PIC XX Value SPACES.
           05 DL-Type            PIC X(5).
           05                    PIC XXX Value SPACES.
           05 DL-Stock           PIC Z999.
           05                    PIC XXX Value SPACES.
           05 DL-Cost            PIC $$$,$$$.99.

       01  Product-Total-Line.
           05                    PIC X(21) Value SPACES.
           05                    PIC X(9) Value 'PRODUCT: '.
           05 PTL-Name           PIC X(13).
           05                    PIC X(10) Value ' TOTAL:   '.
           05 PTL-Cost           PIC $,$$$,$$$.99.

       01  Vendor-Total-Line.
           05                    PIC X(13) Value SPACES.
           05                    PIC X(18) Value 'TOTAL FOR VENDOR: '.
           05 VTL-Name           PIC X(12).
           05                    PIC X(10) Value SPACES.
           05 VTL-Cost           PIC $,$$$,$$$.99.

       01  Warehouse-Total-Line.
           05                    PIC X(10) Value SPACES.
           05                    PIC X(10) Value 'TOTAL FOR '.
           05                    PIC X(11) Value 'WAREHOUSE: '.
           05 WTL-ID             PIC X(4).
           05                    PIC X(17) Value SPACES.
           05 WTL-Cost           PIC $$,$$$,$$$.99.

       01  Grand-Total-Line.
           05                    PIC X(22) Value SPACES.
           05                    PIC X(17) Value 'GRAND TOTAL COST:'.
           05                    PIC X(12) Value SPACES.
           05 GTL-Cost           PIC $$$,$$$,$$$.99.

       PROCEDURE DIVISION.
       
       100-Main-Function.
           PERFORM 125-MergeSort-Function
           PERFORM 1000-End-Function
           PERFORM 200-Housekeeping
           .
      * Sorts all 3 file, then merges them.
       125-MergeSort-Function.
           SORT SortMerge-File
               ON ASCENDING KEY SM-WHID
               ON ASCENDING KEY SM-Vendor
               ON ASCENDING KEY SM-Product
            USING Unsorted-File1
            GIVING Sorted-File1

            SORT SortMerge-File
               ON ASCENDING KEY SM-WHID
               ON ASCENDING KEY SM-Vendor
               ON ASCENDING KEY SM-Product
            USING Unsorted-File2
            GIVING Sorted-File2

            SORT SortMerge-File
               ON ASCENDING KEY SM-WHID
               ON ASCENDING KEY SM-Vendor
               ON ASCENDING KEY SM-Product
            USING Unsorted-File3
            GIVING Sorted-File3

            MERGE SortMerge-File
                ON ASCENDING KEY SM-WHID
                ON ASCENDING KEY SM-Vendor
                ON ASCENDING KEY SM-Product
            USING Sorted-File1, Sorted-File2, Sorted-File3
            GIVING Merged-File
            .

       200-Housekeeping.
           OPEN INPUT Merged-File
           OPEN OUTPUT Inventory-Report
           OPEN OUTPUT Inventory-Errors
           ACCEPT WS-Current-Date FROM DATE
           MOVE WS-Month TO H2-Month
           MOVE WS-Day TO H2-Day
           MOVE WS-Year TO H2-Year
           .

       1000-End-Function.
           CLOSE Merged-File
           CLOSE Inventory-Errors
           CLOSE Inventory-Report
           STOP RUN 
           .

    