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
           05 Valid-WH               PIC X Value 'Y'.

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
           05 T-T                    PIC S9(7)v99.

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
               10 VI-Vendor-Name     PIC X(12).



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
           05 H2-PageNum         PIC 99 Value ZERO.

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
           PERFORM 200-Housekeeping
           PERFORM 300-Read-File
           PERFORM 600-Print-Grand-Total
           PERFORM 1000-End-Function
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
      * Opens the files, gets the date, moves the date to heading 2.
       200-Housekeeping.
           OPEN INPUT Merged-File
           OPEN OUTPUT Inventory-Report
           OPEN OUTPUT Error-File
           ACCEPT WS-Current-Date FROM DATE
           MOVE WS-Month TO H2-Month
           MOVE WS-Day TO H2-Day
           MOVE WS-Year TO H2-Year
           .
      * Writes the page headings to the top of every page.
       225-Page-Headings.
            ADD 1 To H2-PageNum
            WRITE Report-Line From Heading-One
               After Advancing PAGE
            MOVE 1 To Proper-Spacing
            WRITE Report-Line From Heading-Two
                After Advancing Proper-Spacing
            .
      * Writes the warehouse heading.
       250-Print-Warehouse.
           MOVE WH-Hold to WHID-Heading
           MOVE 2 To Proper-Spacing
           WRITE Report-Line From Warehouse-Heading
               After Advancing Proper-Spacing
            MOVE 2 to Proper-Spacing
           .
      * Gets the vendor's full name by searching the table, writes 
      * the vendor heading.
       275-Print-Vendor.
            Set Vendor-Index to 1

            SEARCH Vendor-Item
                AT END
                    STRING 'INVALID' DELIMITED BY ' '
                               ' ' DELIMITED BY SIZE
                            Vendor-Hold DELIMITED BY SIZE
                            INTO VH-Vendor
                    END-STRING
                WHEN Vendor-Hold = Vendor-Key(Vendor-Index)
                   MOVE VI-Vendor-Name(Vendor-Index) To VH-Vendor
            END-SEARCH

            WRITE Report-Line from Vendor-Heading
               After Advancing Proper-Spacing
            MOVE 2 to Proper-Spacing
            .

       280-Print-Detail-Headings.
           WRITE Report-Line from Detail-Heading-One
               After Advancing Proper-Spacing
            MOVE 1 to Proper-Spacing
            Write Report-Line From Detail-Heading-Two
                After Advancing Proper-Spacing
            Move 2 to Proper-Spacing
            .
                
      * Reads the file line by line and calls the function to process
      * the data.
       300-Read-File.
            PERFORM UNTIL More-Data = 'N'
                READ Merged-File
                    AT END
                       MOVE 'N' to More-Data
                    NOT AT END
                        PERFORM 400-Process-File
                END-READ
            END-PERFORM
            .

       400-Process-File.
      * If the previous line didn't have a valid warehouse id, moves the
      * warehouse id from the current line to the warehouse hold.
            IF Valid-WH = 'N' then
                MOVE MR-WHID TO WH-Hold
            END-IF
      * Validates the warehouse id.
            EVALUATE TRUE
                WHEN MR-WHID  = 'CA20'
                   MOVE 'Y' to Valid-WH
                WHEN MR-WHID = 'NV10'
                    MOVE 'Y' to Valid-WH
                WHEN MR-WHID = 'WA30'
                    MOVE 'Y' to Valid-WH
                WHEN OTHER
                    MOVE 'N' to Valid-WH
            END-EVALUATE
      * Checks if it's the first run or if the warehouse/vendor/product
      * has changed and breaks if it has.
            IF Valid-WH = 'Y'
                EVALUATE TRUE
                    WHEN First-Run = 'Y'
                        MOVE 'N' To First-Run
                        MOVE MR-WHID to WH-Hold
                        MOVE MR-Vendor to Vendor-Hold
                        MOVE MR-Product to Product-Hold
                        PERFORM 225-Page-Headings
                        PERFORM 250-Print-Warehouse
                        PERFORM 275-Print-Vendor
                        PERFORM 280-Print-Detail-Headings
                    WHEN WH-Hold NOT = MR-WHID
                       PERFORM 500-Warehouse-Break
                       MOVE MR-WHID to WH-Hold
                       MOVE MR-Vendor to Vendor-Hold
                       MOVE MR-Product to Product-Hold
                       PERFORM 225-Page-Headings
                       PERFORM 250-Print-Warehouse
                       PERFORM 275-Print-Vendor
                       PERFORM 280-Print-Detail-Headings
                    WHEN Vendor-Hold NOT = MR-Vendor
                       PERFORM 525-Vendor-Break
                       MOVE MR-Vendor to Vendor-Hold
                       MOVE MR-Product to Product-Hold
                       PERFORM 275-Print-Vendor
                       PERFORM 280-Print-Detail-Headings
                    WHEN Product-Hold NOT = MR-Product
                       PERFORM 550-Product-Break
                       MOVE MR-Product to Product-Hold
                       PERFORM 280-Print-Detail-Headings
                    END-EVALUATE

                    MOVE 1 to Sub

                    PERFORM UNTIL Sub > 5
      * Validates that the input isn't blank
                        IF MRD-Stock(Sub) NOT = SPACES then
      * Only prints the product name once for each product
                            IF Sub = 1 then
                               MOVE MRD-Name(Sub) to DL-Prod-Name
                            ELSE
                                MOVE SPACES to DL-Prod-Name
                            END-IF
      * Validates/expands the size.
                            EVALUATE TRUE
                                WHEN MRD-Size(Sub) = 'X'
                                    MOVE 'Extra Large' to DL-Size
                                WHEN MRD-Size(Sub) = 'L'
                                    MOVE 'Large' to DL-Size
                                WHEN MRD-Size(Sub) = 'M'
                                    MOVE 'Medium' to DL-Size
                                WHEN MRD-Size(Sub) = 'S'
                                    MOVE 'Small' to DL-Size
                                WHEN MRD-Size(Sub) = 'A'
                                    MOVE 'Sample' to DL-Size
                                WHEN OTHER
                                    STRING 'BAD' DELIMITED BY ' '
                                            ' ' DELIMITED BY SIZE
                                        MRD-Size(Sub) DELIMITED BY SIZE
                                        INTO DL-Size
                                    END-STRING
                            END-EVALUATE
      * Validates/expands the type.
                            EVALUATE TRUE
                                WHEN MRD-Type(Sub) = 'C'
                                    MOVE 'Cream' to DL-Type
                                WHEN MRD-Type(Sub) = 'O'
                                    MOVE 'Oil' to DL-Type
                                WHEN OTHER
                                    STRING 'BAD' DELIMITED BY ' '
                                            ' ' DELIMITED BY SIZE
                                        MRD-Type(Sub) DELIMITED BY SIZE
                                        INTO DL-Size
                                    END-STRING
                            END-EVALUATE
      * Validates the stock and price and adds them to the totals.
                            IF (MRD-Price(Sub) IS NUMERIC) then
                                IF (MRD-Stock(Sub) IS NUMERIC) then
                                 MOVE MRD-Stock(Sub) to DL-Stock
                        COMPUTE T-T = MRD-Price(Sub) * MRD-Stock(Sub)
                                 MOVE T-T to DL-Cost
                                 ADD T-T to Grand-Total
                                 ADD T-T to WH-Total
                                 ADD T-T to Vendor-Total
                                 ADD T-T to Prod-Total
                                 MOVE ZEROS to T-T

                                ELSE
                                    MOVE ZEROS to DL-Stock
                                    MOVE ZEROS to DL-Cost
                                END-IF
                            ELSE
                                MOVE ZEROS to DL-Stock
                                MOVE ZEROS to DL-Cost
                            END-IF

                            WRITE Report-Line from Detail-Line
                               After Advancing Proper-Spacing
                            MOVE 1 to Proper-Spacing      

                        END-IF
                        ADD 1 to Sub
                    END-PERFORM                  
            ELSE 
                ADD 1 to Error-Total
                WRITE Error-Line from Merged-Record
            END-IF
            .
            
       500-Warehouse-Break.
           PERFORM 525-Vendor-Break
           MOVE WH-Hold to WTL-ID
           MOVE WH-Total to WTL-Cost
           WRITE Report-Line from Warehouse-Total-Line
               After Advancing Proper-Spacing
            MOVE 2 to Proper-Spacing
            MOVE ZEROS to WH-Total
            .

       525-Vendor-Break.
            PERFORM 550-Product-Break
            Set Vendor-Index to 1

            SEARCH Vendor-Item
                AT END
                    STRING 'INVALID' DELIMITED BY ' '
                               ' ' DELIMITED BY SIZE
                            Vendor-Hold DELIMITED BY SIZE
                            INTO VTL-Name
                    END-STRING
                WHEN Vendor-Hold = Vendor-Key(Vendor-Index)
                   MOVE VI-Vendor-Name(Vendor-Index) To VTL-Name
            END-SEARCH

            MOVE Vendor-Total to VTL-Cost
            WRITE Report-Line from Vendor-Total-Line
                After Advancing Proper-Spacing
            MOVE 2 to Proper-Spacing
            MOVE ZEROS to Vendor-Total
            .

       550-Product-Break.
            MOVE Product-Hold to PTL-Name
            MOVE Prod-Total to PTL-Cost
            MOVE 2 to Proper-Spacing
            WRITE Report-Line from Product-Total-Line
                After Advancing Proper-Spacing
            MOVE 3 to Proper-Spacing
            MOVE ZEROS to Prod-Total
            .

       600-Print-Grand-Total.
           PERFORM 500-Warehouse-Break
           MOVE Grand-Total to GTL-Cost
           WRITE Report-Line From Grand-Total-Line
               After Advancing Proper-Spacing
            DISPLAY 'There were ' Error-Total ' error(s) in the input'
            .
                

       1000-End-Function.
           CLOSE Merged-File
           CLOSE Error-File
           CLOSE Inventory-Report
           STOP RUN 
           .

    