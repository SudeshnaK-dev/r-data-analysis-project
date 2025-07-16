Customer Segmentation Using SQL (Online Retail Dataset)

Project Overview
This project involves analyzing and segmenting customers based on their purchasing behavior using SQL. The data comes from a UK-based online retail store and includes transaction-level records such as invoice numbers, customer IDs, item quantities, unit prices, invoice dates, and countries.

The objective is to clean the raw data and create a customer-level summary that can be used for further clustering and analysis in R.

Objectives
- Remove invalid or incomplete transaction records.
- Aggregate key metrics for each customer.
- Generate a clean dataset for use in clustering models.
- Gain insights into customer purchase behavior through SQL analysis.

Dataset Information
- Source: Kaggle – Online Retail Dataset
- Total Records: 541,909

Fields Used:
CustomerID, InvoiceNo, Quantity, UnitPrice, InvoiceDate, Country

Data Cleaning Summary
Removed transactions where:
- CustomerID is null or blank.
- Country is null.
- Quantity or UnitPrice is less than or equal to zero.
- Trimmed white spaces from CustomerID to avoid empty string issues.

Features Extracted (Per Customer)
Metric			          Description
total_revenue		      Total amount spent (Quantity × Unit Price)
total_orders		      Number of unique invoices
total_items		        Total quantity of items purchased
avg_order_value	    	Revenue divided by total number of orders
avg_item_price		    Revenue divided by total quantity purchased
first_order_date	    Date of the first purchase
last_order_date		    Date of the most recent purchase
country			          Country of the customer

Sample Output (Top Customers)
CustomerID	Total Revenue	  Orders	Items	  	Avg Order Value		Country
18102		    259,657.53	    60	    64,124		4,327.62		      United Kingdom
17450		    194,550.79	    46	    69,993		4,229.36		      United Kingdom

Tools and Techniques Used
- SQL (SQLite) with DBeaver interface
- Common Table Expressions (CTEs)
- Aggregation functions: SUM(), COUNT(), ROUND(), MIN(), MAX()
- Data filtering using TRIM() and logical conditions

Next Steps
The cleaned and structured dataset will now be imported into R. The next phase involves performing clustering (e.g., K-Means) to identify customer segments such as high spenders, repeat buyers, or price-sensitive customers.

